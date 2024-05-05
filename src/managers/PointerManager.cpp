#include "PointerManager.hpp"
#include "../Compositor.hpp"
#include "../config/ConfigValue.hpp"
#include "../protocols/PointerGestures.hpp"
#include "../protocols/FractionalScale.hpp"
#include <wlr/interfaces/wlr_output.h>
#include <wlr/render/interface.h>
#include <wlr/render/wlr_renderer.h>

// TODO: make nicer
// this will come with the eventual rewrite of wlr_drm, etc...
static bool wlr_drm_format_intersect(wlr_drm_format* dst, const wlr_drm_format* a, const wlr_drm_format* b) {
    ASSERT(a->format == b->format);

    size_t    capacity  = a->len < b->len ? a->len : b->len;
    uint64_t* modifiers = (uint64_t*)malloc(sizeof(*modifiers) * capacity);
    if (!modifiers)
        return false;

    struct wlr_drm_format fmt = {
        .format    = a->format,
        .len       = 0,
        .capacity  = capacity,
        .modifiers = modifiers,
    };

    for (size_t i = 0; i < a->len; i++) {
        for (size_t j = 0; j < b->len; j++) {
            if (a->modifiers[i] == b->modifiers[j]) {
                ASSERT(fmt.len < fmt.capacity);
                fmt.modifiers[fmt.len++] = a->modifiers[i];
                break;
            }
        }
    }

    wlr_drm_format_finish(dst);
    *dst = fmt;
    return true;
}

static bool wlr_drm_format_copy(wlr_drm_format* dst, const wlr_drm_format* src) {
    ASSERT(src->len <= src->capacity);

    uint64_t* modifiers = (uint64_t*)malloc(sizeof(*modifiers) * src->len);
    if (!modifiers)
        return false;

    memcpy(modifiers, src->modifiers, sizeof(*modifiers) * src->len);

    wlr_drm_format_finish(dst);
    dst->capacity  = src->len;
    dst->len       = src->len;
    dst->format    = src->format;
    dst->modifiers = modifiers;
    return true;
}

static const wlr_drm_format_set* wlr_renderer_get_render_formats(wlr_renderer* r) {
    if (!r->impl->get_render_formats)
        return nullptr;

    return r->impl->get_render_formats(r);
}

static bool output_pick_format(wlr_output* output, const wlr_drm_format_set* display_formats, wlr_drm_format* format, uint32_t fmt) {

    const wlr_drm_format_set* render_formats = wlr_renderer_get_render_formats(g_pCompositor->m_sWLRRenderer);
    if (render_formats == NULL) {
        wlr_log(WLR_ERROR, "Failed to get render formats");
        return false;
    }

    const wlr_drm_format* render_format = wlr_drm_format_set_get(render_formats, fmt);
    if (render_format == NULL) {
        wlr_log(WLR_DEBUG, "Renderer doesn't support format 0x%" PRIX32, fmt);
        return false;
    }

    if (display_formats != NULL) {
        const wlr_drm_format* display_format = wlr_drm_format_set_get(display_formats, fmt);
        if (display_format == NULL) {
            wlr_log(WLR_DEBUG, "Output doesn't support format 0x%" PRIX32, fmt);
            return false;
        }
        if (!wlr_drm_format_intersect(format, display_format, render_format)) {
            wlr_log(WLR_DEBUG,
                    "Failed to intersect display and render "
                    "modifiers for format 0x%" PRIX32 " on output %s",
                    fmt, output->name);
            return false;
        }
    } else {
        // The output can display any format
        if (!wlr_drm_format_copy(format, render_format))
            return false;
    }

    if (format->len == 0) {
        wlr_drm_format_finish(format);
        wlr_log(WLR_DEBUG, "Failed to pick output format");
        return false;
    }

    return true;
}

static bool output_pick_cursor_format(struct wlr_output* output, struct wlr_drm_format* format) {
    struct wlr_allocator* allocator = output->allocator;
    ASSERT(allocator != NULL);

    const struct wlr_drm_format_set* display_formats = NULL;
    if (output->impl->get_cursor_formats) {
        display_formats = output->impl->get_cursor_formats(output, allocator->buffer_caps);
        if (display_formats == NULL) {
            wlr_log(WLR_DEBUG, "Failed to get cursor display formats");
            return false;
        }
    }

    return output_pick_format(output, display_formats, format, DRM_FORMAT_ARGB8888);
}

CPointerManager::CPointerManager() {
    hooks.monitorAdded = g_pHookSystem->hookDynamic("newMonitor", [this](void* self, SCallbackInfo& info, std::any data) {
        auto PMONITOR = std::any_cast<SP<CMonitor>>(data);

        onMonitorLayoutChange();

        PMONITOR->events.modeChanged.registerStaticListener([this](void* owner, std::any data) { onMonitorLayoutChange(); }, nullptr);
        PMONITOR->events.disconnect.registerStaticListener([this](void* owner, std::any data) { onMonitorLayoutChange(); }, nullptr);
        PMONITOR->events.destroy.registerStaticListener(
            [this](void* owner, std::any data) { std::erase_if(monitorStates, [](const auto& other) { return other->monitor.expired(); }); }, nullptr);
    });
}

void CPointerManager::lockSoftwareForMonitor(SP<CMonitor> mon) {
    auto state = stateFor(mon);
    state->softwareLocks++;
}

void CPointerManager::unlockSoftwareForMonitor(SP<CMonitor> mon) {
    auto state = stateFor(mon);
    state->softwareLocks--;
    if (state->softwareLocks < 0)
        state->softwareLocks = 0;
}

Vector2D CPointerManager::position() {
    return pointerPos;
}

bool CPointerManager::hasCursor() {
    return currentCursorImage.pBuffer || currentCursorImage.surface;
}

SP<CPointerManager::SMonitorPointerState> CPointerManager::stateFor(SP<CMonitor> mon) {
    auto it = std::find_if(monitorStates.begin(), monitorStates.end(), [mon](const auto& other) { return other->monitor == mon; });
    if (it == monitorStates.end())
        return monitorStates.emplace_back(makeShared<CPointerManager::SMonitorPointerState>(mon));
    return *it;
}

void CPointerManager::setCursorBuffer(wlr_buffer* buf, const Vector2D& hotspot, const float& scale) {
    if (buf == currentCursorImage.pBuffer) {
        if (hotspot != currentCursorImage.hotspot || scale != currentCursorImage.scale) {
            currentCursorImage.hotspot = hotspot;
            currentCursorImage.scale   = scale;
            updateCursorBackend();
        }

        return;
    }

    resetCursorImage();

    if (buf) {
        currentCursorImage.size    = {buf->width, buf->height};
        currentCursorImage.pBuffer = wlr_buffer_lock(buf);

        currentCursorImage.hyprListener_destroyBuffer.initCallback(
            &buf->events.destroy, [this](void* owner, void* data) { resetCursorImage(); }, this, "CPointerManager");
    }

    currentCursorImage.hotspot = hotspot;
    currentCursorImage.scale   = scale;

    updateCursorBackend();
}

void CPointerManager::setCursorSurface(CWLSurface* surf, const Vector2D& hotspot, const float& scale) {
    if (surf == currentCursorImage.surface) {
        if (hotspot != currentCursorImage.hotspot || scale != currentCursorImage.scale) {
            currentCursorImage.hotspot = hotspot;
            currentCursorImage.scale   = scale;
            updateCursorBackend();
        }

        return;
    }

    resetCursorImage();

    if (surf) {
        currentCursorImage.size    = {surf->wlr()->current.buffer_width, surf->wlr()->current.buffer_height};
        currentCursorImage.surface = surf;

        currentCursorImage.destroySurface = surf->events.destroy.registerListener([this](std::any data) { resetCursorImage(); });
        currentCursorImage.hyprListener_commitSurface.initCallback(
            &surf->wlr()->events.commit,
            [this](void* owner, void* data) {
                currentCursorImage.size  = {currentCursorImage.surface->wlr()->current.buffer_width, currentCursorImage.surface->wlr()->current.buffer_height};
                currentCursorImage.scale = currentCursorImage.surface->wlr()->current.scale;
                recheckEnteredOutputs();
                updateCursorBackend();
            },
            nullptr, "CPointerManager");

        if (surf->wlr()->current.buffer) {
            timespec now;
            clock_gettime(CLOCK_MONOTONIC, &now);
            wlr_surface_send_frame_done(surf->wlr(), &now);
        }
    }

    currentCursorImage.hotspot = hotspot;
    currentCursorImage.scale   = scale;

    recheckEnteredOutputs();

    updateCursorBackend();
}

void CPointerManager::recheckEnteredOutputs() {
    if (!currentCursorImage.surface)
        return;

    auto box = getCursorBoxGlobal();

    for (auto& m : g_pCompositor->m_vMonitors) {
        if (!m->m_bEnabled || m->isMirror())
            continue;

        if (box.overlaps({m->vecPosition, m->vecSize}))
            wlr_surface_send_enter(currentCursorImage.surface->wlr(), m->output);
        else
            wlr_surface_send_leave(currentCursorImage.surface->wlr(), m->output);

        PROTO::fractional->sendScale(currentCursorImage.surface->wlr(), m->scale);
        g_pCompositor->setPreferredScaleForSurface(currentCursorImage.surface->wlr(), m->scale);
    }
}

void CPointerManager::resetCursorImage() {
    if (currentCursorImage.surface) {
        for (auto& m : g_pCompositor->m_vMonitors) {
            wlr_surface_send_leave(currentCursorImage.surface->wlr(), m->output);
        }

        currentCursorImage.destroySurface.reset();
        currentCursorImage.hyprListener_commitSurface.removeCallback();
        currentCursorImage.surface = nullptr;
    } else if (currentCursorImage.pBuffer) {
        wlr_buffer_unlock(currentCursorImage.pBuffer);
        currentCursorImage.hyprListener_destroyBuffer.removeCallback();
        currentCursorImage.pBuffer = nullptr;
    }

    for (auto& ms : monitorStates) {
        if (ms->cursorFrontBuffer) {
            if (ms->monitor->output->impl->set_cursor)
                ms->monitor->output->impl->set_cursor(ms->monitor->output, nullptr, 0, 0);
            wlr_buffer_unlock(ms->cursorFrontBuffer);
            ms->cursorFrontBuffer = nullptr;
        }
    }
}

void CPointerManager::updateCursorBackend() {
    if (!hasCursor())
        return;

    static auto PNOHW = CConfigValue<Hyprlang::INT>("cursor:no_hardware_cursors");

    for (auto& m : g_pCompositor->m_vMonitors) {
        auto state = stateFor(m);

        if (state->softwareLocks > 0 || *PNOHW || !attemptHardwareCursor(state)) {
            Debug::log(TRACE, "Output {} rejected hardware cursors, falling back to sw", m->szName);
            state->box            = getCursorBoxLogicalForMonitor(state->monitor.lock());
            state->hardwareFailed = true;
            continue;
        }

        state->hardwareFailed = false;
    }
}

void CPointerManager::onCursorMoved() {
    if (!hasCursor())
        return;

    for (auto& m : g_pCompositor->m_vMonitors) {
        auto state = stateFor(m);

        state->box = getCursorBoxLogicalForMonitor(state->monitor.lock());

        if (state->hardwareFailed)
            continue;

        const auto CURSORBOX = getCursorBoxForMonitor(m).translate(currentCursorImage.hotspot / currentCursorImage.scale * m->scale);
        m->output->impl->move_cursor(m->output, CURSORBOX.x, CURSORBOX.y);
    }
}

bool CPointerManager::attemptHardwareCursor(SP<CPointerManager::SMonitorPointerState> state) {
    auto output = state->monitor->output;

    if (!output->impl->set_cursor)
        return false;

    const auto CURSORBOX = getCursorBoxForMonitor(state->monitor.lock());

    output->impl->move_cursor(output, CURSORBOX.x, CURSORBOX.y);

    auto buffer = renderHWCursorBuffer(state);

    if (!buffer) {
        Debug::log(TRACE, "[pointer] hw cursor failed rendering");
        return false;
    }

    bool success = setHWCursorBuffer(state, buffer);

    return success;
}

bool CPointerManager::setHWCursorBuffer(SP<SMonitorPointerState> state, wlr_buffer* buf) {
    if (!state->monitor->output->impl->set_cursor)
        return false;

    if (!state->monitor->output->impl->set_cursor(state->monitor->output, buf, currentCursorImage.hotspot.x, currentCursorImage.hotspot.y))
        return false;

    wlr_buffer_unlock(state->cursorFrontBuffer);
    state->cursorFrontBuffer = buf;

    if (buf)
        wlr_buffer_lock(buf);

    return true;
}

wlr_buffer* CPointerManager::renderHWCursorBuffer(SP<CPointerManager::SMonitorPointerState> state) {
    auto [texture, textureIsNew] = getCurrentCursorTexture();
    auto output                  = state->monitor->output;

    if (!texture)
        return nullptr;

    int w = currentCursorImage.size.x, h = currentCursorImage.size.y;
    if (output->impl->get_cursor_size) {
        output->impl->get_cursor_size(output, &w, &h);

        if (w < currentCursorImage.size.x || h < currentCursorImage.size.y) {
            Debug::log(TRACE, "hardware cursor too big! {} > {}x{}", currentCursorImage.size, w, h);
            if (textureIsNew)
                wlr_texture_destroy(texture);
            return nullptr;
        }
    }

    if (w <= 0 || h <= 0) {
        Debug::log(TRACE, "hw cursor for output {} failed the size checks ({}x{} is invalid)", state->monitor->szName, w, h);
        return nullptr;
    }

    if (!output->cursor_swapchain || Vector2D{w, h} != Vector2D{output->cursor_swapchain->width, output->cursor_swapchain->height}) {
        wlr_drm_format fmt = {0};
        if (!output_pick_cursor_format(output, &fmt)) {
            Debug::log(TRACE, "Failed to pick cursor format");
            if (textureIsNew)
                wlr_texture_destroy(texture);
            return nullptr;
        }

        wlr_swapchain_destroy(output->cursor_swapchain);
        output->cursor_swapchain = wlr_swapchain_create(output->allocator, w, h, &fmt);
        wlr_drm_format_finish(&fmt);

        if (!output->cursor_swapchain) {
            Debug::log(TRACE, "Failed to create cursor swapchain");
            if (textureIsNew)
                wlr_texture_destroy(texture);
            return nullptr;
        }
    }

    wlr_buffer* buf = wlr_swapchain_acquire(output->cursor_swapchain, nullptr);
    if (!buf)
        return nullptr;

    CRegion damage = {0, 0, INT16_MAX, INT16_MAX};
    g_pHyprRenderer->beginRender(state->monitor.get(), damage, RENDER_MODE_TO_BUFFER, buf);
    g_pHyprOpenGL->clear(CColor{0.F, 0.F, 0.F, 0.F});

    CBox xbox = {{}, Vector2D{currentCursorImage.size / currentCursorImage.scale * state->monitor->scale}.round()};
    Debug::log(TRACE, "[pointer] size: {}, hw buf: {}, scale: {:.2f}, monscale: {:.2f}, xbox: {}", currentCursorImage.size, Vector2D{w, h}, currentCursorImage.scale,
               state->monitor->scale, xbox.size());
    g_pHyprOpenGL->renderTexture(texture, &xbox, 1.F);

    g_pHyprRenderer->endRender();

    wlr_buffer_unlock(buf);

    if (textureIsNew)
        wlr_texture_destroy(texture);

    return buf;
}

void CPointerManager::renderSoftwareCursorsFor(SP<CMonitor> pMonitor, timespec* now, CRegion& damage, std::optional<Vector2D> overridePos) {
    if (!hasCursor())
        return;

    auto state                   = stateFor(pMonitor);
    auto [texture, textureIsNew] = getCurrentCursorTexture();

    if ((!state->hardwareFailed && state->softwareLocks == 0) || !texture) {
        if (textureIsNew)
            wlr_texture_destroy(texture);

        if (currentCursorImage.surface)
            wlr_surface_send_frame_done(currentCursorImage.surface->wlr(), now);
        return;
    }

    if (overridePos.has_value()) {
        auto box = state->box.copy();
        box.x    = overridePos->x;
        box.y    = overridePos->y;
        g_pHyprOpenGL->renderTextureWithDamage(texture, &box, &damage, 1.F);
    } else
        g_pHyprOpenGL->renderTextureWithDamage(texture, &state->box, &damage, 1.F);

    if (currentCursorImage.surface)
        wlr_surface_send_frame_done(currentCursorImage.surface->wlr(), now);

    if (textureIsNew)
        wlr_texture_destroy(texture);

    CBox cursorBox = getCursorBoxGlobal();
    g_pHyprRenderer->damageBox(&cursorBox);
}

CBox CPointerManager::getCursorBoxForMonitor(SP<CMonitor> pMonitor) {
    return getCursorBoxLogicalForMonitor(pMonitor).transform(pMonitor->transform, pMonitor->vecTransformedSize.x, pMonitor->vecTransformedSize.y).scale(pMonitor->scale);
}

CBox CPointerManager::getCursorBoxLogicalForMonitor(SP<CMonitor> pMonitor) {
    return getCursorBoxGlobal().translate(-pMonitor->vecPosition);
}

CBox CPointerManager::getCursorBoxGlobal() {
    return CBox{pointerPos, currentCursorImage.size / currentCursorImage.scale}.translate(-currentCursorImage.hotspot / currentCursorImage.scale);
}

Vector2D CPointerManager::closestValid(const Vector2D& pos) {
    static auto PADDING = CConfigValue<Hyprlang::INT>("cursor:hotspot_padding");

    auto        CURSOR_PADDING = std::clamp((int)*PADDING, 1, 100); // 1px
    CBox        hotBox         = {{pos.x - CURSOR_PADDING, pos.y - CURSOR_PADDING}, {2 * CURSOR_PADDING, 2 * CURSOR_PADDING}};

    //
    static auto INSIDE_LAYOUT = [this](const CBox& box) -> bool {
        for (auto& b : currentMonitorLayout.monitorBoxes) {
            if (box.inside(b))
                return true;
        }
        return false;
    };

    static auto INSIDE_LAYOUT_COORD = [this](const Vector2D& vec) -> bool {
        for (auto& b : currentMonitorLayout.monitorBoxes) {
            if (b.containsPoint(vec))
                return true;
        }
        return false;
    };

    static auto NEAREST_LAYOUT = [this](const Vector2D& vec) -> Vector2D {
        Vector2D leader;
        float    distanceSq = __FLT_MAX__;

        for (auto& b : currentMonitorLayout.monitorBoxes) {
            auto p      = b.closestPoint(vec);
            auto distSq = p.distanceSq(vec);

            if (distSq < distanceSq) {
                leader     = p;
                distanceSq = distSq;
            }
        }

        if (distanceSq > 1337.69420e+20F)
            return {0, 0}; // ???

        return leader;
    };

    if (INSIDE_LAYOUT(hotBox))
        return pos;

    Vector2D leader = NEAREST_LAYOUT(pos);

    hotBox.x = leader.x - CURSOR_PADDING;
    hotBox.y = leader.y - CURSOR_PADDING;

    // push the hotbox around so that it fits in the layout

    if (!INSIDE_LAYOUT_COORD(hotBox.middle() + Vector2D{CURSOR_PADDING, CURSOR_PADDING})) {
        auto delta = NEAREST_LAYOUT(hotBox.middle() + Vector2D{CURSOR_PADDING, CURSOR_PADDING}) - (hotBox.middle() + Vector2D{CURSOR_PADDING, CURSOR_PADDING});
        hotBox.translate(delta);
    }

    if (!INSIDE_LAYOUT_COORD(hotBox.middle() - Vector2D{CURSOR_PADDING, CURSOR_PADDING})) {
        auto delta = NEAREST_LAYOUT(hotBox.middle() - Vector2D{CURSOR_PADDING, CURSOR_PADDING}) - (hotBox.middle() - Vector2D{CURSOR_PADDING, CURSOR_PADDING});
        hotBox.translate(delta);
    }

    if (!INSIDE_LAYOUT_COORD(hotBox.middle() + Vector2D{CURSOR_PADDING, -CURSOR_PADDING})) {
        auto delta = NEAREST_LAYOUT(hotBox.middle() + Vector2D{CURSOR_PADDING, -CURSOR_PADDING}) - (hotBox.middle() + Vector2D{CURSOR_PADDING, -CURSOR_PADDING});
        hotBox.translate(delta);
    }

    if (!INSIDE_LAYOUT_COORD(hotBox.middle() + Vector2D{-CURSOR_PADDING, CURSOR_PADDING})) {
        auto delta = NEAREST_LAYOUT(hotBox.middle() + Vector2D{-CURSOR_PADDING, CURSOR_PADDING}) - (hotBox.middle() + Vector2D{-CURSOR_PADDING, CURSOR_PADDING});
        hotBox.translate(delta);
    }

    return hotBox.middle();
}

void CPointerManager::damageIfSoftware() {
    auto        b = getCursorBoxGlobal();

    static auto PNOHW = CConfigValue<Hyprlang::INT>("cursor:no_hardware_cursors");

    for (auto& mw : monitorStates) {
        if (mw->monitor.expired())
            continue;

        if ((mw->softwareLocks > 0 || mw->hardwareFailed || *PNOHW) && b.overlaps({mw->monitor->vecPosition, mw->monitor->vecSize})) {
            g_pHyprRenderer->damageBox(&b);
            break;
        }
    }
}

void CPointerManager::warpTo(const Vector2D& logical) {
    damageIfSoftware();

    pointerPos = closestValid(logical);
    updateCursorBackend();
    recheckEnteredOutputs();

    damageIfSoftware();
}

void CPointerManager::move(const Vector2D& deltaLogical) {
    const auto oldPos = pointerPos;
    auto       newPos = oldPos + deltaLogical;

    damageIfSoftware();

    pointerPos = closestValid(newPos);
    onCursorMoved();
    recheckEnteredOutputs();

    damageIfSoftware();
}

void CPointerManager::warpAbsolute(const Vector2D& abs, SP<IHID> dev) {

    SP<CMonitor> currentMonitor = g_pCompositor->m_pLastMonitor.lock();

    switch (dev->getType()) {
        case HID_TYPE_TABLET:
            //TODO:
            break;
        case HID_TYPE_TOUCH: {
            auto TOUCH = SP<ITouch>(dev);
            if (!TOUCH->boundOutput.empty()) {
                const auto PMONITOR = g_pCompositor->getMonitorFromString(TOUCH->boundOutput);
                if (PMONITOR)
                    currentMonitor = PMONITOR->self.lock();
            }
            break;
        }
        default: break;
    }

    if (!currentMonitor)
        return;

    damageIfSoftware();

    pointerPos = currentMonitor->vecPosition + currentMonitor->vecSize * abs;
    onCursorMoved();
    recheckEnteredOutputs();

    damageIfSoftware();
}

void CPointerManager::onMonitorLayoutChange() {
    currentMonitorLayout.monitorBoxes.clear();
    for (auto& m : g_pCompositor->m_vMonitors) {
        if (m->isMirror() || !m->m_bEnabled)
            continue;

        currentMonitorLayout.monitorBoxes.emplace_back(CBox{m->vecPosition, m->vecSize});
    }

    damageIfSoftware();

    pointerPos = closestValid(pointerPos);
    updateCursorBackend();
    recheckEnteredOutputs();

    damageIfSoftware();
}

std::pair<wlr_texture*, bool> CPointerManager::getCurrentCursorTexture() {
    if (!currentCursorImage.pBuffer && (!currentCursorImage.surface || !wlr_surface_get_texture(currentCursorImage.surface->wlr())))
        return {nullptr, false};

    return currentCursorImage.pBuffer ? std::pair<wlr_texture*, bool>{wlr_texture_from_buffer(g_pCompositor->m_sWLRRenderer, currentCursorImage.pBuffer), true} :
                                        std::pair<wlr_texture*, bool>{wlr_surface_get_texture(currentCursorImage.surface->wlr()), false};
}

void CPointerManager::attachPointer(SP<IPointer> pointer) {
    if (!pointer)
        return;

    auto listener = pointerListeners.emplace_back(makeShared<SPointerListener>());

    listener->pointer = pointer;

    // clang-format off
    listener->destroy = pointer->events.destroy.registerListener([this] (std::any d) {
        detachPointer(nullptr);
    });

    listener->motion = pointer->pointerEvents.motion.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SMotionEvent>(e);

        g_pInputManager->onMouseMoved(E);
    });

    listener->motionAbsolute = pointer->pointerEvents.motionAbsolute.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SMotionAbsoluteEvent>(e);

        g_pInputManager->onMouseWarp(E);
    });

    listener->button = pointer->pointerEvents.button.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SButtonEvent>(e);

        g_pInputManager->onMouseButton(E);
    });

    listener->axis = pointer->pointerEvents.axis.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SAxisEvent>(e);

        g_pInputManager->onMouseWheel(E);
    });

    listener->frame = pointer->pointerEvents.frame.registerListener([this] (std::any e) {
        wlr_seat_pointer_notify_frame(g_pCompositor->m_sSeat.seat);
    });

    listener->swipeBegin = pointer->pointerEvents.swipeBegin.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SSwipeBeginEvent>(e);

        g_pInputManager->onSwipeBegin(E);
    });

    listener->swipeEnd = pointer->pointerEvents.swipeEnd.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SSwipeEndEvent>(e);

        g_pInputManager->onSwipeEnd(E);
    });

    listener->swipeUpdate = pointer->pointerEvents.swipeUpdate.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SSwipeUpdateEvent>(e);

        g_pInputManager->onSwipeUpdate(E);
    });

    listener->pinchBegin = pointer->pointerEvents.pinchBegin.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SPinchBeginEvent>(e);

        PROTO::pointerGestures->pinchBegin(E.timeMs, E.fingers);
    });

    listener->pinchEnd = pointer->pointerEvents.pinchEnd.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SPinchEndEvent>(e);

        PROTO::pointerGestures->pinchEnd(E.timeMs, E.cancelled);
    });

    listener->pinchUpdate = pointer->pointerEvents.pinchUpdate.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SPinchUpdateEvent>(e);

        PROTO::pointerGestures->pinchUpdate(E.timeMs, E.delta, E.scale, E.rotation);
    });

    listener->holdBegin = pointer->pointerEvents.holdBegin.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SHoldBeginEvent>(e);

        PROTO::pointerGestures->holdBegin(E.timeMs, E.fingers);
    });

    listener->holdEnd = pointer->pointerEvents.holdEnd.registerListener([this] (std::any e) {
        auto E = std::any_cast<IPointer::SHoldEndEvent>(e);

        PROTO::pointerGestures->holdEnd(E.timeMs, E.cancelled);
    });
    // clang-format on

    Debug::log(LOG, "Attached pointer {} to global", pointer->hlName);
}

void CPointerManager::attachTouch(SP<ITouch> touch) {
    if (!touch)
        return;

    auto listener = touchListeners.emplace_back(makeShared<STouchListener>());

    listener->touch = touch;

    // clang-format off
    listener->destroy = touch->events.destroy.registerListener([this] (std::any d) {
        detachTouch(nullptr);
    });

    listener->down = touch->touchEvents.down.registerListener([this] (std::any e) {
        auto E = std::any_cast<ITouch::SDownEvent>(e);

        g_pInputManager->onTouchDown(E);
    });

    listener->up = touch->touchEvents.up.registerListener([this] (std::any e) {
        auto E = std::any_cast<ITouch::SUpEvent>(e);

        g_pInputManager->onTouchUp(E);
    });

    listener->motion = touch->touchEvents.motion.registerListener([this] (std::any e) {
        auto E = std::any_cast<ITouch::SMotionEvent>(e);

        g_pInputManager->onTouchMove(E);
    });

    listener->cancel = touch->touchEvents.cancel.registerListener([this] (std::any e) {
        //
    });

    listener->frame = touch->touchEvents.frame.registerListener([this] (std::any e) {
        wlr_seat_touch_notify_frame(g_pCompositor->m_sSeat.seat);
    });
    // clang-format on

    Debug::log(LOG, "Attached touch {} to global", touch->hlName);
}

void CPointerManager::detachPointer(SP<IPointer> pointer) {
    std::erase_if(pointerListeners, [pointer](const auto& e) { return e->pointer.expired() || e->pointer == pointer; });
}

void CPointerManager::detachTouch(SP<ITouch> touch) {
    std::erase_if(touchListeners, [touch](const auto& e) { return e->touch.expired() || e->touch == touch; });
}

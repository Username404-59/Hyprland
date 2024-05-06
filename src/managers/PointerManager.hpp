#pragma once

#include "../devices/IPointer.hpp"
#include "../devices/ITouch.hpp"
#include "../devices/Tablet.hpp"
#include "../helpers/Box.hpp"
#include "../helpers/Region.hpp"
#include "../desktop/WLSurface.hpp"
#include <tuple>

class CMonitor;
struct wlr_input_device;
class IHID;

/*
    The naming here is a bit confusing.
    CPointerManager manages the _position_ and _displaying_ of the cursor,
    but the CCursorManager _only_ manages the actual image (texture) and size
    of the cursor.
*/

class CPointerManager {
  public:
    CPointerManager();

    void attachPointer(SP<IPointer> pointer);
    void attachTouch(SP<ITouch> touch);
    void attachTablet(SP<CTablet> tablet);

    void detachPointer(SP<IPointer> pointer);
    void detachTouch(SP<ITouch> touch);
    void detachTablet(SP<CTablet> tablet);

    // only clamps to the layout.
    void warpTo(const Vector2D& logical);
    void move(const Vector2D& deltaLogical);
    void warpAbsolute(Vector2D abs, SP<IHID> dev);

    void setCursorBuffer(wlr_buffer* buf, const Vector2D& hotspot, const float& scale);
    void setCursorSurface(CWLSurface* buf, const Vector2D& hotspot, const float& scale);
    void resetCursorImage();

    void lockSoftwareForMonitor(SP<CMonitor> pMonitor);
    void unlockSoftwareForMonitor(SP<CMonitor> pMonitor);

    void renderSoftwareCursorsFor(SP<CMonitor> pMonitor, timespec* now, CRegion& damage /* logical */, std::optional<Vector2D> overridePos = {} /* monitor-local */);

    //
    Vector2D position();

  private:
    void recheckPointerPosition();
    void onMonitorLayoutChange();
    void onMonitorDisconnect();
    void updateCursorBackend();
    void onCursorMoved();
    bool hasCursor();
    void damageIfSoftware();
    void recheckEnteredOutputs();

    // closest valid point to a given one
    Vector2D closestValid(const Vector2D& pos);

    // returns the thing in device coordinates.
    CBox getCursorBoxForMonitor(SP<CMonitor> pMonitor);
    // returns the thing in logical coordinates of the monitor
    CBox getCursorBoxLogicalForMonitor(SP<CMonitor> pMonitor);
    // returns the thing in global coords
    CBox getCursorBoxGlobal();

    // texture and bool needsToDestroy
    std::pair<wlr_texture*, bool> getCurrentCursorTexture();

    struct SPointerListener {
        CHyprSignalListener destroy;
        CHyprSignalListener motion;
        CHyprSignalListener motionAbsolute;
        CHyprSignalListener button;
        CHyprSignalListener axis;
        CHyprSignalListener frame;

        CHyprSignalListener swipeBegin;
        CHyprSignalListener swipeEnd;
        CHyprSignalListener swipeUpdate;

        CHyprSignalListener pinchBegin;
        CHyprSignalListener pinchEnd;
        CHyprSignalListener pinchUpdate;

        CHyprSignalListener holdBegin;
        CHyprSignalListener holdEnd;

        WP<IPointer>        pointer;
    };
    std::vector<SP<SPointerListener>> pointerListeners;

    struct STouchListener {
        CHyprSignalListener destroy;
        CHyprSignalListener down;
        CHyprSignalListener up;
        CHyprSignalListener motion;
        CHyprSignalListener cancel;
        CHyprSignalListener frame;

        WP<ITouch>          touch;
    };
    std::vector<SP<STouchListener>> touchListeners;

    struct STabletListener {
        CHyprSignalListener destroy;
        CHyprSignalListener axis;
        CHyprSignalListener proximity;
        CHyprSignalListener tip;
        CHyprSignalListener button;

        WP<CTablet>         tablet;
    };
    std::vector<SP<STabletListener>> tabletListeners;

    struct {
        std::vector<CBox> monitorBoxes;
    } currentMonitorLayout;

    struct {
        wlr_buffer*         pBuffer = nullptr;
        CWLSurface*         surface = nullptr;

        Vector2D            hotspot;
        Vector2D            size;
        float               scale = 1.F;

        CHyprSignalListener destroySurface;
        DYNLISTENER(commitSurface);
        DYNLISTENER(destroyBuffer);
    } currentCursorImage; // TODO: support various sizes per-output so we can have pixel-perfect cursors

    Vector2D pointerPos = {0, 0};

    struct SMonitorPointerState {
        SMonitorPointerState(SP<CMonitor> m) : monitor(m) {}
        WP<CMonitor> monitor;

        int          softwareLocks  = 0;
        bool         hardwareFailed = false;
        CBox         box; // logical

        wlr_buffer*  cursorFrontBuffer = nullptr;
    };

    std::vector<SP<SMonitorPointerState>> monitorStates;
    SP<SMonitorPointerState>              stateFor(SP<CMonitor> mon);
    bool                                  attemptHardwareCursor(SP<SMonitorPointerState> state);
    wlr_buffer*                           renderHWCursorBuffer(SP<SMonitorPointerState> state);
    bool                                  setHWCursorBuffer(SP<SMonitorPointerState> state, wlr_buffer* buf);

    struct {
        SP<HOOK_CALLBACK_FN> monitorAdded;
    } hooks;
};

inline UP<CPointerManager> g_pPointerManager;

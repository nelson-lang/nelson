/*
  WebUI Library
  Win32 WebView2 C++ Implementation
  https://webui.me
  https://github.com/webui-dev/webui
  Copyright (c) 2020-2026 Hassan Draga.
  Licensed under MIT License.
  All rights reserved.
  Canada.
*/

#ifdef _WIN32

#include "win32_wv2.hpp"
#include "WebView2.h"
#include <cstdlib>
#include <cstdio>

#ifdef _MSC_VER
// MSVC: Use Windows Runtime Library
#include <wrl.h>
using namespace Microsoft::WRL;
#else
// MinGW: Define our own ComPtr template
template <typename T> class ComPtr
{
    T* ptr;

public:
    ComPtr() : ptr(nullptr) { }
    ~ComPtr() { Reset(); }
    ComPtr(const ComPtr&) = delete;
    ComPtr&
    operator=(const ComPtr&)
        = delete;

    void
    Reset()
    {
        if (ptr) {
            ptr->Release();
            ptr = nullptr;
        }
    }

    T*
    Get() const
    {
        return ptr;
    }
    T**
    GetAddressOf()
    {
        Reset();
        return &ptr;
    }
    T*
    operator->() const
    {
        return ptr;
    }
    operator bool() const { return ptr != nullptr; }

    ComPtr&
    operator=(T* p)
    {
        Reset();
        ptr = p;
        if (ptr) {
            ptr->AddRef();
        }
        return *this;
    }
};
#endif

class WebView2Instance
{
public:
    ComPtr<ICoreWebView2Environment> webviewEnvironment;
    ComPtr<ICoreWebView2Controller> webviewController;
    ComPtr<ICoreWebView2> webviewWindow;
    HWND hwnd;
    wchar_t* url;
    bool navigate, size, position, stop;
    bool environmentCompleted, controllerCompleted;
    HRESULT environmentResult, controllerResult;
    int width, height, x, y;
    // NELSON PATCH: transient-retry state for CreateCoreWebView2Controller
    // (0x8007139F ERROR_INVALID_STATE) against the shared environment.
    bool needsControllerRetry;
    int controllerRetries;

    WebView2Instance()
        : hwnd(nullptr)
        , url(nullptr)
        , navigate(false)
        , size(false)
        , position(false)
        , stop(false)
        , environmentCompleted(false)
        , controllerCompleted(false)
        , environmentResult(S_OK)
        , controllerResult(S_OK)
        , width(800)
        , height(600)
        , x(0)
        , y(0)
        , needsControllerRetry(false)
        , controllerRetries(0)
    {
    }

    ~WebView2Instance()
    {
        if (url) {
            free(url);
        }
    }
};

static bool
nelsonWebUiDebugEnabled()
{
    static const bool enabled = []() {
        const char* value = std::getenv("NELSON_WEBUI_DEBUG");
        return value && value[0] == '1';
    }();
    return enabled;
}

static void
nelsonWebUiDebugHr(const char* message, HRESULT hr)
{
    if (!nelsonWebUiDebugEnabled()) {
        return;
    }
    std::fprintf(
        stderr, "Nelson WebUI debug: %s hr=0x%08lx\n", message, static_cast<unsigned long>(hr));
    std::fflush(stderr);
}

static void
nelsonWebUiDebugLastError(const char* message)
{
    if (!nelsonWebUiDebugEnabled()) {
        return;
    }
    std::fprintf(stderr, "Nelson WebUI debug: %s error=%lu\n", message,
        static_cast<unsigned long>(GetLastError()));
    std::fflush(stderr);
}

static void
disableWebViewZoomControls(ICoreWebView2Settings* settings)
{
    if (!settings) {
        return;
    }
    settings->put_IsZoomControlEnabled(FALSE);
    ICoreWebView2Settings5* settings5 = nullptr;
    if (SUCCEEDED(settings->QueryInterface(IID_ICoreWebView2Settings5, (void**)&settings5))
        && settings5) {
        settings5->put_IsPinchZoomEnabled(FALSE);
        settings5->Release();
    }
}

static void
resetWebViewZoom(WebView2Instance* instance)
{
    if (!instance || !instance->webviewController) {
        return;
    }
    double zoomFactor = 1.0;
    if (SUCCEEDED(instance->webviewController->get_ZoomFactor(&zoomFactor))
        && (zoomFactor < 0.999 || zoomFactor > 1.001)) {
        instance->webviewController->put_ZoomFactor(1.0);
    }
}

// NELSON PATCH: the pages served by the local Nelson server are trusted, so
// clipboard-read requests from them are granted silently instead of showing
// the WebView2 permission prompt (whose "Block" answer would silently disable
// paste from the console context menu).
static bool
nelsonIsTrustedLocalUri(LPCWSTR uri)
{
    if (!uri) {
        return false;
    }
    static const wchar_t* prefixes[]
        = { L"http://localhost", L"https://localhost", L"http://127.0.0.1", L"https://127.0.0.1" };
    for (const wchar_t* prefix : prefixes) {
        const size_t len = wcslen(prefix);
        if (wcsncmp(uri, prefix, len) == 0) {
            const wchar_t next = uri[len];
            if (next == L'\0' || next == L':' || next == L'/') {
                return true;
            }
        }
    }
    return false;
}

static void
nelsonHandlePermissionRequested(ICoreWebView2PermissionRequestedEventArgs* args)
{
    if (!args) {
        return;
    }
    COREWEBVIEW2_PERMISSION_KIND kind = COREWEBVIEW2_PERMISSION_KIND_UNKNOWN_PERMISSION;
    if (FAILED(args->get_PermissionKind(&kind))
        || kind != COREWEBVIEW2_PERMISSION_KIND_CLIPBOARD_READ) {
        return;
    }
    LPWSTR uri = nullptr;
    if (SUCCEEDED(args->get_Uri(&uri)) && uri) {
        if (nelsonIsTrustedLocalUri(uri)) {
            args->put_State(COREWEBVIEW2_PERMISSION_STATE_ALLOW);
        }
        CoTaskMemFree(uri);
    }
}

#ifdef _MSC_VER
// MSVC: Use WRL RuntimeClass
class TitleChangedHandler : public RuntimeClass<RuntimeClassFlags<ClassicCom>,
                                ICoreWebView2DocumentTitleChangedEventHandler>
{
    WebView2Instance* instance;

public:
    TitleChangedHandler(WebView2Instance* inst) : instance(inst) { }
    HRESULT STDMETHODCALLTYPE
    Invoke(ICoreWebView2* sender, IUnknown* args) override
    {
        (void)args;
        if (sender && instance->hwnd) {
            LPWSTR newTitle = nullptr;
            sender->get_DocumentTitle(&newTitle);
            if (newTitle) {
                SetWindowTextW(instance->hwnd, newTitle);
                CoTaskMemFree(newTitle);
            }
        }
        return S_OK;
    }
};

class ZoomChangedHandler
    : public RuntimeClass<RuntimeClassFlags<ClassicCom>, ICoreWebView2ZoomFactorChangedEventHandler>
{
    WebView2Instance* instance;

public:
    ZoomChangedHandler(WebView2Instance* inst) : instance(inst) { }
    HRESULT STDMETHODCALLTYPE
    Invoke(ICoreWebView2Controller* sender, IUnknown* args) override
    {
        (void)sender;
        (void)args;
        resetWebViewZoom(instance);
        return S_OK;
    }
};
#else
// MinGW: Manual COM implementation
class TitleChangedHandler : public ICoreWebView2DocumentTitleChangedEventHandler
{
    ULONG refCount;
    WebView2Instance* instance;

public:
    TitleChangedHandler(WebView2Instance* inst) : refCount(1), instance(inst) { }
    virtual ~TitleChangedHandler() = default;

    HRESULT STDMETHODCALLTYPE
    QueryInterface(REFIID riid, void** ppvObject) override
    {
        if (!ppvObject) {
            return E_POINTER;
        }
        *ppvObject = nullptr;
        if (IsEqualIID(riid, IID_IUnknown)
            || IsEqualIID(riid, IID_ICoreWebView2DocumentTitleChangedEventHandler)) {
            *ppvObject = static_cast<ICoreWebView2DocumentTitleChangedEventHandler*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE
    AddRef() override
    {
        return ++refCount;
    }

    ULONG STDMETHODCALLTYPE
    Release() override
    {
        ULONG count = --refCount;
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE
    Invoke(ICoreWebView2* sender, IUnknown* args) override
    {
        (void)args;
        if (sender && instance->hwnd) {
            LPWSTR newTitle = nullptr;
            sender->get_DocumentTitle(&newTitle);
            if (newTitle) {
                SetWindowTextW(instance->hwnd, newTitle);
                CoTaskMemFree(newTitle);
            }
        }
        return S_OK;
    }
};

class ZoomChangedHandler : public ICoreWebView2ZoomFactorChangedEventHandler
{
    ULONG refCount;
    WebView2Instance* instance;

public:
    ZoomChangedHandler(WebView2Instance* inst) : refCount(1), instance(inst) { }
    virtual ~ZoomChangedHandler() = default;

    HRESULT STDMETHODCALLTYPE
    QueryInterface(REFIID riid, void** ppvObject) override
    {
        if (!ppvObject) {
            return E_POINTER;
        }
        *ppvObject = nullptr;
        if (IsEqualIID(riid, IID_IUnknown)
            || IsEqualIID(riid, IID_ICoreWebView2ZoomFactorChangedEventHandler)) {
            *ppvObject = static_cast<ICoreWebView2ZoomFactorChangedEventHandler*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE
    AddRef() override
    {
        return ++refCount;
    }

    ULONG STDMETHODCALLTYPE
    Release() override
    {
        ULONG count = --refCount;
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE
    Invoke(ICoreWebView2Controller* sender, IUnknown* args) override
    {
        (void)sender;
        (void)args;
        resetWebViewZoom(instance);
        return S_OK;
    }
};
#endif

// NELSON PATCH: silent clipboard-read grant for the local Nelson pages.
#ifdef _MSC_VER
class PermissionRequestedHandler : public RuntimeClass<RuntimeClassFlags<ClassicCom>,
                                       ICoreWebView2PermissionRequestedEventHandler>
{
public:
    HRESULT STDMETHODCALLTYPE
    Invoke(ICoreWebView2* sender, ICoreWebView2PermissionRequestedEventArgs* args) override
    {
        (void)sender;
        nelsonHandlePermissionRequested(args);
        return S_OK;
    }
};
#else
class PermissionRequestedHandler : public ICoreWebView2PermissionRequestedEventHandler
{
    ULONG refCount;

public:
    PermissionRequestedHandler() : refCount(1) { }
    virtual ~PermissionRequestedHandler() = default;

    HRESULT STDMETHODCALLTYPE
    QueryInterface(REFIID riid, void** ppvObject) override
    {
        if (!ppvObject) {
            return E_POINTER;
        }
        *ppvObject = nullptr;
        if (IsEqualIID(riid, IID_IUnknown)
            || IsEqualIID(riid, IID_ICoreWebView2PermissionRequestedEventHandler)) {
            *ppvObject = static_cast<ICoreWebView2PermissionRequestedEventHandler*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE
    AddRef() override
    {
        return ++refCount;
    }

    ULONG STDMETHODCALLTYPE
    Release() override
    {
        ULONG count = --refCount;
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE
    Invoke(ICoreWebView2* sender, ICoreWebView2PermissionRequestedEventArgs* args) override
    {
        (void)sender;
        nelsonHandlePermissionRequested(args);
        return S_OK;
    }
};
#endif

#ifdef _MSC_VER
class ControllerCompletedHandler : public RuntimeClass<RuntimeClassFlags<ClassicCom>,
                                       ICoreWebView2CreateCoreWebView2ControllerCompletedHandler>
{
    WebView2Instance* instance;

public:
    ControllerCompletedHandler(WebView2Instance* inst) : instance(inst) { }
    HRESULT STDMETHODCALLTYPE
    Invoke(HRESULT result, ICoreWebView2Controller* controller) override
    {
        nelsonWebUiDebugHr("CreateCoreWebView2Controller completed", result);
        // NELSON PATCH: the shared environment can transiently answer with
        // 0x8007139F (ERROR_INVALID_STATE) when a controller is created right
        // after another was created + navigated on the same environment.
        // Flag a retry instead of reporting a hard failure, so has_failed
        // stays false and the host thread re-issues the request.
        if (FAILED(result) && result == HRESULT_FROM_WIN32(ERROR_INVALID_STATE)
            && instance->controllerRetries < 30) {
            instance->controllerRetries++;
            instance->needsControllerRetry = true;
            return S_OK;
        }
        instance->controllerCompleted = true;
        instance->controllerResult = result;
        if (SUCCEEDED(result) && controller) {
            instance->webviewController = controller;
            controller->get_CoreWebView2(&instance->webviewWindow);

            ComPtr<ICoreWebView2Settings> settings;
            if (SUCCEEDED(instance->webviewWindow->get_Settings(&settings))) {
                settings->put_IsScriptEnabled(TRUE);
                settings->put_AreDefaultScriptDialogsEnabled(TRUE);
                settings->put_IsWebMessageEnabled(TRUE);
#ifdef WEBUI_LOG
                settings->put_AreDevToolsEnabled(TRUE);
#else
                settings->put_AreDevToolsEnabled(FALSE);
#endif
                disableWebViewZoomControls(settings.Get());
            }

            RECT bounds = { 0, 0, instance->width, instance->height };
            if (instance->hwnd) {
                GetClientRect(instance->hwnd, &bounds);
            }
            controller->put_Bounds(bounds);
            controller->put_ZoomFactor(1.0);
            auto zoomHandler = Make<ZoomChangedHandler>(instance);
            EventRegistrationToken zoomToken;
            controller->add_ZoomFactorChanged(zoomHandler.Get(), &zoomToken);

            auto titleHandler = Make<TitleChangedHandler>(instance);
            EventRegistrationToken token;
            instance->webviewWindow->add_DocumentTitleChanged(titleHandler.Get(), &token);

            // NELSON PATCH: grant clipboard read silently for local pages.
            auto permissionHandler = Make<PermissionRequestedHandler>();
            EventRegistrationToken permissionToken;
            instance->webviewWindow->add_PermissionRequested(
                permissionHandler.Get(), &permissionToken);

            if (instance->url) {
                instance->webviewWindow->Navigate(instance->url);
            }
        }
        return S_OK;
    }
};
#else
class ControllerCompletedHandler : public ICoreWebView2CreateCoreWebView2ControllerCompletedHandler
{
    ULONG refCount;
    WebView2Instance* instance;

public:
    ControllerCompletedHandler(WebView2Instance* inst) : refCount(1), instance(inst) { }
    virtual ~ControllerCompletedHandler() = default;

    HRESULT STDMETHODCALLTYPE
    QueryInterface(REFIID riid, void** ppvObject) override
    {
        if (!ppvObject) {
            return E_POINTER;
        }
        *ppvObject = nullptr;
        if (IsEqualIID(riid, IID_IUnknown)
            || IsEqualIID(riid, IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandler)) {
            *ppvObject
                = static_cast<ICoreWebView2CreateCoreWebView2ControllerCompletedHandler*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE
    AddRef() override
    {
        return ++refCount;
    }

    ULONG STDMETHODCALLTYPE
    Release() override
    {
        ULONG count = --refCount;
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE
    Invoke(HRESULT result, ICoreWebView2Controller* controller) override
    {
        nelsonWebUiDebugHr("CreateCoreWebView2Controller completed", result);
        // NELSON PATCH: the shared environment can transiently answer with
        // 0x8007139F (ERROR_INVALID_STATE) when a controller is created right
        // after another was created + navigated on the same environment.
        // Flag a retry instead of reporting a hard failure, so has_failed
        // stays false and the host thread re-issues the request.
        if (FAILED(result) && result == HRESULT_FROM_WIN32(ERROR_INVALID_STATE)
            && instance->controllerRetries < 30) {
            instance->controllerRetries++;
            instance->needsControllerRetry = true;
            return S_OK;
        }
        instance->controllerCompleted = true;
        instance->controllerResult = result;
        if (SUCCEEDED(result) && controller) {
            instance->webviewController = controller;
            controller->get_CoreWebView2(instance->webviewWindow.GetAddressOf());

            ICoreWebView2Settings* settings = nullptr;
            if (SUCCEEDED(instance->webviewWindow->get_Settings(&settings)) && settings) {
                settings->put_IsScriptEnabled(TRUE);
                settings->put_AreDefaultScriptDialogsEnabled(TRUE);
                settings->put_IsWebMessageEnabled(TRUE);
#ifdef WEBUI_LOG
                settings->put_AreDevToolsEnabled(TRUE);
#else
                settings->put_AreDevToolsEnabled(FALSE);
#endif
                disableWebViewZoomControls(settings);
                settings->Release();
            }

            RECT bounds = { 0, 0, instance->width, instance->height };
            if (instance->hwnd) {
                GetClientRect(instance->hwnd, &bounds);
            }
            controller->put_Bounds(bounds);
            controller->put_ZoomFactor(1.0);
            ZoomChangedHandler* zoomHandler = new ZoomChangedHandler(instance);
            EventRegistrationToken zoomToken;
            controller->add_ZoomFactorChanged(zoomHandler, &zoomToken);

            TitleChangedHandler* titleHandler = new TitleChangedHandler(instance);
            EventRegistrationToken token;
            instance->webviewWindow->add_DocumentTitleChanged(titleHandler, &token);

            // NELSON PATCH: grant clipboard read silently for local pages.
            PermissionRequestedHandler* permissionHandler = new PermissionRequestedHandler();
            EventRegistrationToken permissionToken;
            instance->webviewWindow->add_PermissionRequested(permissionHandler, &permissionToken);

            if (instance->url) {
                instance->webviewWindow->Navigate(instance->url);
            }
        }
        return S_OK;
    }
};
#endif

#ifdef _MSC_VER
class EnvironmentCompletedHandler : public RuntimeClass<RuntimeClassFlags<ClassicCom>,
                                        ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler>
{
    WebView2Instance* instance;

public:
    EnvironmentCompletedHandler(WebView2Instance* inst) : instance(inst) { }
    HRESULT STDMETHODCALLTYPE
    Invoke(HRESULT result, ICoreWebView2Environment* env) override
    {
        instance->environmentCompleted = true;
        instance->environmentResult = result;
        nelsonWebUiDebugHr("CreateCoreWebView2Environment completed", result);
        if (SUCCEEDED(result) && env) {
            instance->webviewEnvironment = env;
            auto controllerHandler = Make<ControllerCompletedHandler>(instance);
            HRESULT controllerHr
                = env->CreateCoreWebView2Controller(instance->hwnd, controllerHandler.Get());
            nelsonWebUiDebugHr("CreateCoreWebView2Controller started", controllerHr);
            if (FAILED(controllerHr)) {
                instance->controllerCompleted = true;
                instance->controllerResult = controllerHr;
            }
        }
        return S_OK;
    }
};
#else
class EnvironmentCompletedHandler
    : public ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler
{
    ULONG refCount;
    WebView2Instance* instance;

public:
    EnvironmentCompletedHandler(WebView2Instance* inst) : refCount(1), instance(inst) { }
    virtual ~EnvironmentCompletedHandler() = default;

    HRESULT STDMETHODCALLTYPE
    QueryInterface(REFIID riid, void** ppvObject) override
    {
        if (!ppvObject) {
            return E_POINTER;
        }
        *ppvObject = nullptr;
        if (IsEqualIID(riid, IID_IUnknown)
            || IsEqualIID(riid, IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler)) {
            *ppvObject
                = static_cast<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE
    AddRef() override
    {
        return ++refCount;
    }

    ULONG STDMETHODCALLTYPE
    Release() override
    {
        ULONG count = --refCount;
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE
    Invoke(HRESULT result, ICoreWebView2Environment* env) override
    {
        instance->environmentCompleted = true;
        instance->environmentResult = result;
        nelsonWebUiDebugHr("CreateCoreWebView2Environment completed", result);
        if (SUCCEEDED(result) && env) {
            instance->webviewEnvironment = env;
            ControllerCompletedHandler* controllerHandler
                = new ControllerCompletedHandler(instance);
            HRESULT controllerHr
                = env->CreateCoreWebView2Controller(instance->hwnd, controllerHandler);
            nelsonWebUiDebugHr("CreateCoreWebView2Controller started", controllerHr);
            if (FAILED(controllerHr)) {
                instance->controllerCompleted = true;
                instance->controllerResult = controllerHr;
            }
        }
        return S_OK;
    }
};
#endif

// NELSON PATCH: one WebView2 environment shared by every native window, created
// once on the webview host thread. All controllers are created from it, so every
// figure window lives in a single browser process on a single user-data folder.
static ComPtr<ICoreWebView2Environment> g_sharedEnv;
static int g_sharedEnvState = 0; // 0 none, 1 pending, 2 ready, 3 failed
static HRESULT g_sharedEnvResult = S_OK;
// NELSON PATCH: shared browser process, awaited at teardown to free the user-data folder.
static HANDLE g_browserProcess = nullptr;

#ifdef _MSC_VER
class SharedEnvironmentCompletedHandler
    : public RuntimeClass<RuntimeClassFlags<ClassicCom>,
          ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler>
{
public:
    SharedEnvironmentCompletedHandler() { }
    HRESULT STDMETHODCALLTYPE
    Invoke(HRESULT result, ICoreWebView2Environment* env) override
    {
        g_sharedEnvResult = result;
        nelsonWebUiDebugHr("Shared CreateCoreWebView2Environment completed", result);
        if (SUCCEEDED(result) && env) {
            g_sharedEnv = env;
            g_sharedEnvState = 2;
        } else {
            g_sharedEnvState = 3;
        }
        return S_OK;
    }
};
#else
class SharedEnvironmentCompletedHandler
    : public ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler
{
    ULONG refCount;

public:
    SharedEnvironmentCompletedHandler() : refCount(1) { }
    virtual ~SharedEnvironmentCompletedHandler() = default;

    HRESULT STDMETHODCALLTYPE
    QueryInterface(REFIID riid, void** ppvObject) override
    {
        if (!ppvObject) {
            return E_POINTER;
        }
        *ppvObject = nullptr;
        if (IsEqualIID(riid, IID_IUnknown)
            || IsEqualIID(riid, IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler)) {
            *ppvObject
                = static_cast<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler*>(this);
            AddRef();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    ULONG STDMETHODCALLTYPE
    AddRef() override
    {
        return ++refCount;
    }

    ULONG STDMETHODCALLTYPE
    Release() override
    {
        ULONG count = --refCount;
        if (count == 0) {
            delete this;
        }
        return count;
    }

    HRESULT STDMETHODCALLTYPE
    Invoke(HRESULT result, ICoreWebView2Environment* env) override
    {
        g_sharedEnvResult = result;
        nelsonWebUiDebugHr("Shared CreateCoreWebView2Environment completed", result);
        if (SUCCEEDED(result) && env) {
            g_sharedEnv = env;
            g_sharedEnvState = 2;
        } else {
            g_sharedEnvState = 3;
        }
        return S_OK;
    }
};
#endif

#ifdef WEBUI_WEBVIEW_STATIC
// The WebView2LoaderStatic.lib is linked, no need to load WebView2Loader.dll
#else
// Cached WebView2Loader.dll handle and function pointer
static HMODULE g_webviewLib = NULL;
typedef HRESULT(__stdcall* CreateCoreWebView2EnvironmentWithOptionsFunc)(PCWSTR, PCWSTR,
    ICoreWebView2EnvironmentOptions*, ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler*);
static CreateCoreWebView2EnvironmentWithOptionsFunc g_createEnv = NULL;
#endif

extern "C"
{

    bool
    _webui_win32_wv2_check_loader_dll(void)
    {
#ifdef WEBUI_WEBVIEW_STATIC
        // The WebView2LoaderStatic.lib is linked, no need to load WebView2Loader.dll
        return true;
#else
        // Already loaded and cached
        if (g_webviewLib && g_createEnv) {
            return true;
        }
        // Load and cache WebView2Loader.dll
        g_webviewLib = LoadLibraryA("WebView2Loader.dll");
        if (!g_webviewLib) {
            nelsonWebUiDebugLastError("LoadLibraryA(WebView2Loader.dll) failed");
            return false;
        }
        g_createEnv = (CreateCoreWebView2EnvironmentWithOptionsFunc)GetProcAddress(
            g_webviewLib, "CreateCoreWebView2EnvironmentWithOptions");
        if (!g_createEnv) {
            nelsonWebUiDebugLastError(
                "GetProcAddress(CreateCoreWebView2EnvironmentWithOptions) failed");
            FreeLibrary(g_webviewLib);
            g_webviewLib = NULL;
            return false;
        }
        return true;
#endif
    }

    bool
    _webui_win32_wv2_check_runtime(void)
    {
        // Ensure loader DLL (or static lib) is available first
        if (!_webui_win32_wv2_check_loader_dll()) {
            return false;
        }
        // Verify the WebView2 Runtime is installed by requesting its version string
        typedef HRESULT(__stdcall * GetVersionFunc)(PCWSTR, LPWSTR*);
#ifdef WEBUI_WEBVIEW_STATIC
        GetVersionFunc getVersion = GetAvailableCoreWebView2BrowserVersionString;
#else
        GetVersionFunc getVersion = (GetVersionFunc)GetProcAddress(
            g_webviewLib, "GetAvailableCoreWebView2BrowserVersionString");
        if (!getVersion) {
            nelsonWebUiDebugLastError(
                "GetProcAddress(GetAvailableCoreWebView2BrowserVersionString) failed");
            return false;
        }
#endif
        LPWSTR versionInfo = NULL;
        HRESULT hr = getVersion(NULL, &versionInfo);
        nelsonWebUiDebugHr("GetAvailableCoreWebView2BrowserVersionString", hr);
        if (SUCCEEDED(hr) && versionInfo) {
            CoTaskMemFree(versionInfo);
            return true;
        }
        return false;
    }

    _webui_win32_wv2_handle
    _webui_win32_wv2_create(void)
    {
        return static_cast<_webui_win32_wv2_handle>(new WebView2Instance());
    }

    void
    _webui_win32_wv2_free(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        // NELSON PATCH: remember the shared browser process so teardown can wait for its exit.
        if (!g_browserProcess && instance->webviewWindow) {
            UINT32 browserPid = 0;
            if (SUCCEEDED(instance->webviewWindow->get_BrowserProcessId(&browserPid))
                && browserPid != 0) {
                g_browserProcess = OpenProcess(SYNCHRONIZE, FALSE, browserPid);
            }
        }
        // NELSON PATCH: close the controller so WebView2 releases the user-data folder promptly.
        if (instance->webviewController) {
            instance->webviewController->Close();
        }
        if (instance->webviewWindow) {
            instance->webviewWindow.Reset();
        }
        if (instance->webviewController) {
            instance->webviewController.Reset();
        }
        if (instance->webviewEnvironment) {
            instance->webviewEnvironment.Reset();
        }
        delete instance;
    }

    bool
    _webui_win32_wv2_navigate(_webui_win32_wv2_handle handle, wchar_t* url)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        if (instance->webviewWindow && url) {
            return SUCCEEDED(instance->webviewWindow->Navigate(url));
        }
        return false;
    }

    bool
    _webui_win32_wv2_set_position(_webui_win32_wv2_handle handle, int x, int y)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        return instance->hwnd
            && SetWindowPos(instance->hwnd, NULL, x, y, 0, 0, SWP_NOSIZE | SWP_NOZORDER);
    }

    bool
    _webui_win32_wv2_set_size(_webui_win32_wv2_handle handle, int width, int height)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        return instance->hwnd
            && SetWindowPos(
                instance->hwnd, NULL, 0, 0, width, height, SWP_NOMOVE | SWP_NOREPOSITION);
    }

    bool
    _webui_win32_wv2_maximize(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        if (!instance->hwnd) {
            return false;
        }
        return PostMessage(instance->hwnd, WEBUI_WM_APPLY_GEOMETRY_AND_MAXIMIZE, 0, 0);
    }

    bool
    _webui_win32_wv2_minimize(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        return instance->hwnd && ShowWindow(instance->hwnd, SW_MINIMIZE);
    }

    // NELSON PATCH: hide/show an already-shown window (post-show).
    bool
    _webui_win32_wv2_set_hidden(_webui_win32_wv2_handle handle, bool hidden)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        if (!instance->hwnd) {
            return false;
        }
        ShowWindow(instance->hwnd, hidden ? SW_HIDE : SW_SHOW);
        return true;
    }

    HWND
    _webui_win32_wv2_get_hwnd(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->hwnd : nullptr;
    }

    void
    _webui_win32_wv2_set_hwnd(_webui_win32_wv2_handle handle, HWND hwnd)
    {
        if (handle) {
            static_cast<WebView2Instance*>(handle)->hwnd = hwnd;
        }
    }

    void
    _webui_win32_wv2_set_stop(_webui_win32_wv2_handle handle, bool stop)
    {
        if (handle) {
            static_cast<WebView2Instance*>(handle)->stop = stop;
        }
    }

    bool
    _webui_win32_wv2_get_stop(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->stop : false;
    }

    void
    _webui_win32_wv2_set_navigate_flag(_webui_win32_wv2_handle handle, bool navigate)
    {
        if (handle) {
            static_cast<WebView2Instance*>(handle)->navigate = navigate;
        }
    }

    void
    _webui_win32_wv2_set_size_flag(_webui_win32_wv2_handle handle, bool size)
    {
        if (handle) {
            static_cast<WebView2Instance*>(handle)->size = size;
        }
    }

    void
    _webui_win32_wv2_set_position_flag(_webui_win32_wv2_handle handle, bool position)
    {
        if (handle) {
            static_cast<WebView2Instance*>(handle)->position = position;
        }
    }

    bool
    _webui_win32_wv2_get_navigate_flag(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->navigate : false;
    }

    bool
    _webui_win32_wv2_get_size_flag(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->size : false;
    }

    bool
    _webui_win32_wv2_get_position_flag(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->position : false;
    }

    void
    _webui_win32_wv2_set_url(_webui_win32_wv2_handle handle, wchar_t* url)
    {
        if (!handle) {
            return;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        if (instance->url) {
            free(instance->url);
        }
        if (url) {
            size_t len = wcslen(url) + 1;
            instance->url = (wchar_t*)malloc(len * sizeof(wchar_t));
            if (instance->url) {
                wcscpy_s(instance->url, len, url);
            }
        } else {
            instance->url = nullptr;
        }
    }

    wchar_t*
    _webui_win32_wv2_get_url(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->url : nullptr;
    }

    void
    _webui_win32_wv2_set_dimensions(
        _webui_win32_wv2_handle handle, int x, int y, int width, int height)
    {
        if (!handle) {
            return;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        instance->x = x;
        instance->y = y;
        instance->width = width;
        instance->height = height;
    }

    void
    _webui_win32_wv2_get_dimensions(
        _webui_win32_wv2_handle handle, int* x, int* y, int* width, int* height)
    {
        if (!handle) {
            return;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        if (x) {
            *x = instance->x;
        }
        if (y) {
            *y = instance->y;
        }
        if (width) {
            *width = instance->width;
        }
        if (height) {
            *height = instance->height;
        }
    }

    bool
    _webui_win32_wv2_create_environment(_webui_win32_wv2_handle handle, wchar_t* cacheFolder)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);

        // Ensure loader is ready (DLL or static)
        if (!_webui_win32_wv2_check_loader_dll()) {
            instance->environmentCompleted = true;
            instance->environmentResult = HRESULT_FROM_WIN32(ERROR_MOD_NOT_FOUND);
            return false;
        }

        _wputenv(L"WEBVIEW2_ADDITIONAL_BROWSER_ARGUMENTS="
                 L"--enable-features=msWebView2EnableDraggableRegions "
                 L"--disable-translate "
                 L"--disable-features=Translate,TranslateUI");

#ifdef _MSC_VER
        auto environmentHandler = Make<EnvironmentCompletedHandler>(instance);
        if (!environmentHandler) {
            return false;
        }
#ifdef WEBUI_WEBVIEW_STATIC
        HRESULT hr = CreateCoreWebView2EnvironmentWithOptions(
            NULL, cacheFolder, NULL, environmentHandler.Get());
#else
        HRESULT hr = g_createEnv(NULL, cacheFolder, NULL, environmentHandler.Get());
#endif
#else
        EnvironmentCompletedHandler* environmentHandler = new EnvironmentCompletedHandler(instance);
        if (!environmentHandler) {
            return false;
        }
#ifdef WEBUI_WEBVIEW_STATIC
        HRESULT hr
            = CreateCoreWebView2EnvironmentWithOptions(NULL, cacheFolder, NULL, environmentHandler);
#else
        HRESULT hr = g_createEnv(NULL, cacheFolder, NULL, environmentHandler);
#endif
#endif

        nelsonWebUiDebugHr("CreateCoreWebView2Environment started", hr);
        if (FAILED(hr)) {
            instance->environmentCompleted = true;
            instance->environmentResult = hr;
        }
        return SUCCEEDED(hr);
    }

    // NELSON PATCH: shared WebView2 environment. Only called on the host thread.
    bool
    _webui_win32_wv2_ensure_shared_environment(wchar_t* cacheFolder)
    {
        if (g_sharedEnvState == 2) {
            return true; // ready
        }
        if (g_sharedEnvState == 1) {
            return true; // pending, creation already started
        }
        if (g_sharedEnvState == 3) {
            return false; // permanently failed
        }

        // Ensure loader is ready (DLL or static)
        if (!_webui_win32_wv2_check_loader_dll()) {
            g_sharedEnvResult = HRESULT_FROM_WIN32(ERROR_MOD_NOT_FOUND);
            g_sharedEnvState = 3;
            return false;
        }

        _wputenv(L"WEBVIEW2_ADDITIONAL_BROWSER_ARGUMENTS="
                 L"--enable-features=msWebView2EnableDraggableRegions "
                 L"--disable-translate "
                 L"--disable-features=Translate,TranslateUI");

        g_sharedEnvState = 1;

#ifdef _MSC_VER
        auto environmentHandler = Make<SharedEnvironmentCompletedHandler>();
        if (!environmentHandler) {
            g_sharedEnvState = 3;
            return false;
        }
#ifdef WEBUI_WEBVIEW_STATIC
        HRESULT hr = CreateCoreWebView2EnvironmentWithOptions(
            NULL, cacheFolder, NULL, environmentHandler.Get());
#else
        HRESULT hr = g_createEnv(NULL, cacheFolder, NULL, environmentHandler.Get());
#endif
#else
        SharedEnvironmentCompletedHandler* environmentHandler
            = new SharedEnvironmentCompletedHandler();
#ifdef WEBUI_WEBVIEW_STATIC
        HRESULT hr
            = CreateCoreWebView2EnvironmentWithOptions(NULL, cacheFolder, NULL, environmentHandler);
#else
        HRESULT hr = g_createEnv(NULL, cacheFolder, NULL, environmentHandler);
#endif
#endif

        nelsonWebUiDebugHr("Shared CreateCoreWebView2Environment started", hr);
        if (FAILED(hr)) {
            g_sharedEnvResult = hr;
            g_sharedEnvState = 3;
            return false;
        }
        return true;
    }

    int
    _webui_win32_wv2_shared_environment_state(void)
    {
        return g_sharedEnvState;
    }

    bool
    _webui_win32_wv2_create_controller(_webui_win32_wv2_handle handle)
    {
        if (!handle || g_sharedEnvState != 2 || !g_sharedEnv) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        instance->needsControllerRetry = false;
#ifdef _MSC_VER
        auto controllerHandler = Make<ControllerCompletedHandler>(instance);
        HRESULT hr
            = g_sharedEnv->CreateCoreWebView2Controller(instance->hwnd, controllerHandler.Get());
#else
        ControllerCompletedHandler* controllerHandler = new ControllerCompletedHandler(instance);
        HRESULT hr = g_sharedEnv->CreateCoreWebView2Controller(instance->hwnd, controllerHandler);
#endif
        nelsonWebUiDebugHr("Shared CreateCoreWebView2Controller started", hr);
        if (FAILED(hr)) {
            instance->controllerCompleted = true;
            instance->controllerResult = hr;
            return false;
        }
        return true;
    }

    // NELSON PATCH: does the last controller attempt need a retry? The shared
    // environment can transiently answer CreateCoreWebView2Controller with
    // 0x8007139F (ERROR_INVALID_STATE) when a controller was just created and
    // navigated on the same environment (e.g. the desktop console window right
    // before the first figure window). The completion handler flags this instead of
    // reporting a hard failure, so has_failed stays false and the caller keeps
    // waiting while the host thread re-issues the request.
    bool
    _webui_win32_wv2_needs_controller_retry(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return false;
        }
        return static_cast<WebView2Instance*>(handle)->needsControllerRetry;
    }

    void
    _webui_win32_wv2_release_shared_environment(void)
    {
        if (g_sharedEnv) {
            g_sharedEnv.Reset();
        }
        g_sharedEnvState = 0;
        g_sharedEnvResult = S_OK;
        // NELSON PATCH: wait for the browser process to exit so the user-data folder is unlocked before deletion.
        if (g_browserProcess) {
            WaitForSingleObject(g_browserProcess, 3000);
            CloseHandle(g_browserProcess);
            g_browserProcess = nullptr;
        }
    }

    void*
    _webui_win32_wv2_get_controller(_webui_win32_wv2_handle handle)
    {
        return handle ? static_cast<WebView2Instance*>(handle)->webviewController.Get() : nullptr;
    }

    bool
    _webui_win32_wv2_is_ready(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return false;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        return instance->webviewController && instance->webviewWindow;
    }

    bool
    _webui_win32_wv2_has_failed(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return true;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        return (instance->environmentCompleted && FAILED(instance->environmentResult))
            || (instance->controllerCompleted && FAILED(instance->controllerResult));
    }

    HRESULT
    _webui_win32_wv2_get_last_error(_webui_win32_wv2_handle handle)
    {
        if (!handle) {
            return E_POINTER;
        }
        WebView2Instance* instance = static_cast<WebView2Instance*>(handle);
        if (FAILED(instance->controllerResult)) {
            return instance->controllerResult;
        }
        return instance->environmentResult;
    }
}

#endif

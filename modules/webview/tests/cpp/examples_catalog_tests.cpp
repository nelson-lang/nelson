//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ExamplesBridge.hpp"
#include "ExamplesCatalog.hpp"
#include <nlohmann/json.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <regex>
//=============================================================================
namespace fs = std::filesystem;
using json = nlohmann::json;
//=============================================================================
static bool
expect(bool condition, const std::string& message)
{
    if (!condition) {
        std::cerr << message << '\n';
    }
    return condition;
}
//=============================================================================
static void
write(const fs::path& path, const std::string& text)
{
    fs::create_directories(path.parent_path());
    std::ofstream stream(path);
    stream << text;
}
//=============================================================================
static bool
testValidManifest(const fs::path& root)
{
    const fs::path moduleRoot = root / "valid";
    write(moduleRoot / "examples" / "first.m", "disp('first');");
    write(moduleRoot / "examples" / "second.m", "disp('second');");
    write(moduleRoot / "module.json",
        R"({"title":{"en_US":"External title","fr_FR":"Titre externe"}})");
    write(moduleRoot / "examples" / "index.json",
        R"({"schemaVersion":1,"examples":[{"file":"first.m","title":{"en_US":"First","fr_FR":"Premier"},"description":{"en_US":"One","fr_FR":"Un"},"tags":["a"],"requirements":["gui"]},{"file":"second.m","title":"Second","description":"Two"}]})");
    const Nelson::ExamplesCatalog catalog
        = Nelson::BuildExamplesCatalog({ { L"external", moduleRoot.wstring(), true } }, "fr_FR");
    const json parsed = json::parse(catalog.json);
    return expect(parsed["diagnostics"].empty(), "valid manifest produced a diagnostic")
        && expect(parsed["groups"].size() == 1, "valid module was not cataloged")
        && expect(
            parsed["groups"][0]["title"] == "Titre externe", "toolbox title was not localized")
        && expect(parsed["groups"][0]["examples"][0]["title"] == "Premier",
            "example title was not localized")
        && expect(parsed["groups"][0]["examples"][0]["description"] == "Un",
            "example description was not localized")
        && expect(parsed["groups"][0]["examples"][1]["title"] == "Second", "manifest order changed")
        && expect(catalog.allowedPaths.size() == 2, "allowlist size is incorrect");
}
//=============================================================================
static bool
testInvalidManifest(const fs::path& root, const std::string& name, const std::string& manifest)
{
    const fs::path moduleRoot = root / name;
    write(moduleRoot / "examples" / "demo.m", "disp('demo');");
    write(moduleRoot / "examples" / "index.json", manifest);
    const std::wstring vocabulary
        = (fs::path(NELSON_SOURCE_ROOT) / "etc" / "gallery_requirements.json").wstring();
    const Nelson::ExamplesCatalog catalog = Nelson::BuildExamplesCatalog(
        { { fs::path(name).wstring(), moduleRoot.wstring(), false } }, "en_US", {}, vocabulary);
    const json parsed = json::parse(catalog.json);
    return expect(parsed["groups"].empty(), name + " was not rejected")
        && expect(parsed["diagnostics"].size() == 1, name + " has no diagnostic")
        && expect(catalog.allowedPaths.empty(), name + " leaked an allowed path")
        && expect(catalog.allowedCommands.empty(), name + " leaked an allowed command");
}
//=============================================================================
static bool
testCommandManifest(const fs::path& root)
{
    const fs::path moduleRoot = root / "command-module";
    write(moduleRoot / "examples" / "regular.m", "disp('regular');");
    write(moduleRoot / "examples" / "index.json",
        R"json({"schemaVersion":1,"examples":[{"command":"morse.practice()","title":{"en_US":"Practice","fr_FR":"Entrainement"},"description":"Interactive trainer","requirements":["gui"]},{"file":"regular.m","title":"Regular","description":"Script"}]})json");
    const Nelson::ExamplesCatalog catalog = Nelson::BuildExamplesCatalog(
        { { L"command-module", moduleRoot.wstring(), false } }, "en_US");
    const json parsed = json::parse(catalog.json);
    bool ok = expect(parsed["diagnostics"].empty(), "command manifest produced a diagnostic");
    ok = expect(parsed["groups"].size() == 1, "command module was not cataloged") && ok;
    const json& examples = parsed["groups"][0]["examples"];
    ok = expect(examples.size() == 2, "command manifest lost an entry") && ok;
    ok = expect(examples[0].contains("command") && examples[0]["command"] == "morse.practice()",
             "command entry did not expose its command")
        && ok;
    ok = expect(examples[0]["title"] == "Practice", "command entry title was not localized") && ok;
    ok = expect(!examples[0].contains("file") && !examples[0].contains("relativeFile"),
             "command entry leaked a file path")
        && ok;
    ok = expect(examples[1].contains("file") && !examples[1].contains("command"),
             "file entry was altered by command support")
        && ok;
    ok = expect(catalog.allowedCommands.size() == 1, "command allowlist size is incorrect") && ok;
    ok = expect(catalog.allowedCommands.empty() ? false
                                                : catalog.allowedCommands[0] == L"morse.practice()",
             "command allowlist content is incorrect")
        && ok;
    ok = expect(catalog.allowedPaths.size() == 1, "file allowlist size is incorrect") && ok;
    return ok;
}
//=============================================================================
static bool
testCommandBridge()
{
    std::wstring command;
    std::string error;
    const std::vector<std::wstring> allowed { L"morse.practice()" };
    bool ok = expect(Nelson::BuildExamplesCommandFromCatalogCommand(
                         "run", L"morse.practice()", allowed, command, error)
            && command == L"morse.practice()",
        "cataloged command was not returned verbatim");
    ok = expect(!Nelson::BuildExamplesCommandFromCatalogCommand(
                    "run", L"delete_everything()", allowed, command, error),
             "uncataloged command was accepted")
        && ok;
    ok = expect(!Nelson::BuildExamplesCommandFromCatalogCommand(
                    "open", L"morse.practice()", allowed, command, error),
             "command entry was openable in the editor")
        && ok;
    return ok;
}
//=============================================================================
static bool
testBridge(const fs::path& root)
{
    const fs::path file = fs::weakly_canonical(root / "valid" / "examples" / "first.m");
    std::wstring command;
    std::string error;
    bool ok
        = Nelson::BuildExamplesCommand("run", file.wstring(), { file.wstring() }, command, error);
    ok = expect(ok && command.rfind(L"run('", 0) == 0, "run command was not built") && ok;
    ok = expect(!Nelson::BuildExamplesCommand(
                    "run", (root / "invalid.m").wstring(), { file.wstring() }, command, error),
             "uncataloged path was accepted")
        && ok;
    ok = expect(!Nelson::BuildExamplesCommand(
                    "system", file.wstring(), { file.wstring() }, command, error),
             "arbitrary method was accepted")
        && ok;
    const fs::path quoted = root / "valid" / "examples" / "it's-safe.m";
    write(quoted, "disp('safe');");
    const fs::path canonicalQuoted = fs::weakly_canonical(quoted);
    ok = expect(Nelson::BuildExamplesCommand("open", canonicalQuoted.wstring(),
                    { canonicalQuoted.wstring() }, command, error)
                 && command.find(L"it''s-safe.m") != std::wstring::npos,
             "apostrophe was not escaped")
        && ok;
    const fs::path injection = root / "valid" / "examples" / "x');system('bad.m";
    write(injection, "disp('safe');");
    const fs::path canonicalInjection = fs::weakly_canonical(injection);
    std::wstring escapedInjection = canonicalInjection.wstring();
    size_t quote = 0;
    while ((quote = escapedInjection.find(L'\'', quote)) != std::wstring::npos) {
        escapedInjection.insert(quote, 1, L'\'');
        quote += 2;
    }
    ok = expect(Nelson::BuildExamplesCommand("run", canonicalInjection.wstring(),
                    { canonicalInjection.wstring() }, command, error)
                 && command == L"run('" + escapedInjection + L"')",
             "quoted command text was not kept inside the path literal")
        && ok;
    ok = expect(!Nelson::BuildExamplesCommand("run", (root / L"line\nbreak.m").wstring(),
                    { (root / L"line\nbreak.m").wstring() }, command, error),
             "line break in a command path was accepted")
        && ok;
    return ok;
}
//=============================================================================
static bool
testAbsentManifest(const fs::path& root)
{
    const fs::path moduleRoot = root / "without-manifest";
    write(moduleRoot / "examples" / "hidden.m", "disp('hidden');");
    const Nelson::ExamplesCatalog catalog = Nelson::BuildExamplesCatalog(
        { { L"without-manifest", moduleRoot.wstring(), false } }, "en_US");
    const json parsed = json::parse(catalog.json);
    return expect(parsed["groups"].empty(), "module without manifest was discovered")
        && expect(parsed["diagnostics"].empty(), "absent manifest produced a diagnostic");
}
//=============================================================================
// A malformed example is reported and skipped; the valid examples of the same
// module stay, and a rejected entry leaks no allowed path.
static bool
testResilientManifest(const fs::path& root)
{
    const fs::path moduleRoot = root / "resilient";
    write(moduleRoot / "examples" / "good.m", "disp('good');");
    write(moduleRoot / "examples" / "index.json",
        R"({"schemaVersion":1,"examples":[)"
        R"({"file":"good.m","title":"Good","description":"Valid"},)"
        R"({"file":"good.m","title":"Dup","description":"Duplicate file"},)"
        R"({"file":"missing.m","title":"Missing","description":"No such file"},)"
        R"({"title":"NoTarget","description":"Neither file nor command"}]})");
    const Nelson::ExamplesCatalog catalog
        = Nelson::BuildExamplesCatalog({ { L"resilient", moduleRoot.wstring(), false } }, "en_US");
    const json parsed = json::parse(catalog.json);
    bool ok = expect(parsed["groups"].size() == 1, "valid examples were dropped with the bad ones");
    if (parsed["groups"].empty()) {
        return false;
    }
    ok = expect(parsed["groups"][0]["examples"].size() == 1, "the single valid example was lost")
        && ok;
    ok = expect(parsed["groups"][0]["examples"][0]["title"] == "Good",
             "the kept example is not the valid one")
        && ok;
    ok = expect(parsed["diagnostics"].size() == 3, "each rejected example was not reported") && ok;
    ok = expect(catalog.allowedPaths.size() == 1, "a rejected example leaked an allowed path")
        && ok;
    return ok;
}
//=============================================================================
// An external toolbox is maintained outside this repository, so an unknown
// requirement token is kept as a label instead of dropping the whole toolbox
// from the gallery. An in-tree module stays strict (see "bad-requirement").
static bool
testExternalUnknownRequirement(const fs::path& root)
{
    const fs::path moduleRoot = root / "external_toolbox";
    write(moduleRoot / "examples" / "demo.m", "disp('demo');");
    write(moduleRoot / "examples" / "index.json",
        R"({"schemaVersion":1,"examples":[{"file":"demo.m","title":"Demo","description":"Demo","requirements":["graphics"]}]})");
    const std::wstring vocabulary
        = (fs::path(NELSON_SOURCE_ROOT) / "etc" / "gallery_requirements.json").wstring();
    const Nelson::ExamplesCatalog catalog = Nelson::BuildExamplesCatalog(
        { { L"external_toolbox", moduleRoot.wstring(), true } }, "en_US", {}, vocabulary);
    const json parsed = json::parse(catalog.json);
    bool ok = expect(
        parsed["diagnostics"].empty(), "external toolbox was dropped over an unknown requirement");
    ok = expect(parsed["groups"].size() == 1, "external toolbox was not cataloged") && ok;
    if (parsed["groups"].empty()) {
        return false;
    }
    const json& examples = parsed["groups"][0]["examples"];
    ok = expect(examples.size() == 1 && examples[0]["requirements"].size() == 1
                 && examples[0]["requirements"][0] == "graphics",
             "external requirement label was not kept")
        && ok;
    return ok;
}
//=============================================================================
static bool
testExcludedNflowModule(const fs::path& root)
{
    const fs::path moduleRoot = root / "nflow_custom";
    write(moduleRoot / "examples" / "demo.m", "disp('hidden');");
    write(moduleRoot / "examples" / "index.json",
        R"({"schemaVersion":1,"examples":[{"file":"demo.m","title":"Demo","description":"Demo"}]})");
    const Nelson::ExamplesCatalog catalog = Nelson::BuildExamplesCatalog(
        { { L"nflow_custom", moduleRoot.wstring(), true } }, "en_US");
    const json parsed = json::parse(catalog.json);
    return expect(parsed["groups"].empty(), "NFlow module was included in the generic gallery")
        && expect(parsed["diagnostics"].empty(), "excluded NFlow module produced a diagnostic");
}
//=============================================================================
static bool
testEscapingSymlink(const fs::path& root)
{
    const fs::path moduleRoot = root / "symlink";
    const fs::path outside = root / "outside.m";
    write(outside, "disp('outside');");
    fs::create_directories(moduleRoot / "examples");
    std::error_code error;
    fs::create_symlink(outside, moduleRoot / "examples" / "linked.m", error);
    if (error) {
        return true;
    }
    write(moduleRoot / "examples" / "index.json",
        R"({"schemaVersion":1,"examples":[{"file":"linked.m","title":"Demo","description":"Demo"}]})");
    const Nelson::ExamplesCatalog catalog
        = Nelson::BuildExamplesCatalog({ { L"symlink", moduleRoot.wstring(), false } }, "en_US");
    const json parsed = json::parse(catalog.json);
    return expect(parsed["groups"].empty(), "symlink escaping the module was accepted")
        && expect(catalog.allowedPaths.empty(), "escaping symlink leaked into the allowlist");
}
//=============================================================================
static bool
testProviderGroups(const fs::path& root)
{
    const fs::path model = root / "provider" / "PID_Demo.nflow";
    const fs::path script = root / "provider" / "brake.m";
    write(model, "{\"blocks\":[]}");
    write(script, "disp('brake');");
    const std::string groups = json::array(
        { { { "name", "nflow:causal" }, { "title", "Causal" }, { "source", "nflow" },
              { "examples",
                  json::array({ { { "title", "PID" }, { "description", "Loop" },
                                    { "kind", "model" }, { "file", model.string() },
                                    { "tags", json::array({ "causal" }) }, { "blockCount", 6 } },
                      { { "title", "Brake" }, { "kind", "script" }, { "file", script.string() } },
                      { { "title", "Trainer" }, { "kind", "command" },
                          { "command", "morse.practice()" } } }) } },
            { { "name", "nflow:broken" }, { "title", "Broken" }, { "source", "nflow" },
                { "examples",
                    json::array({ { { "title", "Missing" }, { "kind", "model" },
                        { "file", (root / "provider" / "missing.nflow").string() } } }) } },
            { { "name", "nflow:odd" }, { "title", "Odd" }, { "source", "elsewhere" },
                { "examples", json::array() } } })
                                   .dump();
    const Nelson::ExamplesCatalog catalog
        = Nelson::BuildExamplesCatalog({}, "en_US", { groups, "not json", "{}" });
    const json parsed = json::parse(catalog.json);
    bool ok = expect(parsed["groups"].size() == 1, "valid provider group was not cataloged");
    ok = expect(parsed["diagnostics"].size() == 4,
             "invalid provider groups and texts were not all reported")
        && ok;
    if (parsed["groups"].empty()) {
        return false;
    }
    const json& group = parsed["groups"][0];
    ok = expect(group["source"] == "nflow" && group["name"] == "nflow:causal",
             "provider group identity was altered")
        && ok;
    const json& examples = group["examples"];
    ok = expect(examples.size() == 3, "provider group lost an entry") && ok;
    ok = expect(examples[0]["kind"] == "model" && examples[0]["blockCount"] == 6
                 && examples[0]["tags"].size() == 1,
             "model entry fields were not kept")
        && ok;
    ok = expect(examples[1]["kind"] == "script" && examples[1]["description"] == ""
                 && examples[1]["tags"].empty() && examples[1]["requirements"].empty(),
             "script entry defaults were not filled")
        && ok;
    ok = expect(examples[2]["kind"] == "command" && !examples[2].contains("file"),
             "command entry was not normalized")
        && ok;
    ok = expect(catalog.allowedPaths.size() == 2, "provider files were not allowlisted") && ok;
    ok = expect(catalog.allowedCommands.size() == 1
                 && catalog.allowedCommands[0] == L"morse.practice()",
             "provider command was not allowlisted")
        && ok;
    return ok;
}
//=============================================================================
static bool
testModelBridge(const fs::path& root)
{
    const fs::path model = fs::weakly_canonical(root / "provider" / "PID_Demo.nflow");
    std::wstring command;
    std::string error;
    bool ok = expect(
        Nelson::BuildExamplesCommand("open", model.wstring(), { model.wstring() }, command, error)
            && command == L"nflow('" + model.wstring() + L"')",
        "model open does not launch NFlow");
    ok = expect(Nelson::BuildExamplesCommand(
                    "run", model.wstring(), { model.wstring() }, command, error)
                 && command.rfind(L"nflow('", 0) == 0,
             "model run does not launch NFlow")
        && ok;
    return ok;
}
//=============================================================================
static bool
containsSessionTermination(const std::string& source);
//=============================================================================
static bool
testRepositoryManifests()
{
    const fs::path modulesRoot = fs::path(NELSON_SOURCE_ROOT) / "modules";
    std::vector<Nelson::ExamplesModule> modules;
    for (const auto& entry : fs::directory_iterator(modulesRoot)) {
        if (entry.is_directory()) {
            modules.push_back({ entry.path().filename().wstring(), entry.path().wstring(), false });
        }
    }
    const std::wstring vocabulary
        = (fs::path(NELSON_SOURCE_ROOT) / "etc" / "gallery_requirements.json").wstring();
    const Nelson::ExamplesCatalog catalog
        = Nelson::BuildExamplesCatalog(modules, "en_US", {}, vocabulary);
    const json parsed = json::parse(catalog.json);
    if (!parsed["diagnostics"].empty()) {
        std::cerr << parsed["diagnostics"].dump(2) << '\n';
    }
    bool ok
        = expect(parsed["diagnostics"].empty(), "repository contains invalid example manifests");
    for (const std::wstring& allowedPath : catalog.allowedPaths) {
        std::ifstream stream { fs::path(allowedPath) };
        const std::string source((std::istreambuf_iterator<char>(stream)), {});
        ok = expect(!containsSessionTermination(source),
                 "cataloged example contains a forced termination")
            && ok;
    }
    return ok;
}
//=============================================================================
static std::string
readSource(const fs::path& path)
{
    std::ifstream stream(path);
    return { std::istreambuf_iterator<char>(stream), std::istreambuf_iterator<char>() };
}
//=============================================================================
static bool
containsSessionTermination(const std::string& source)
{
    static const std::regex expression(
        R"((^|[;,\r\n][ \t]*)(exit|quit)[ \t]*(\(|;|\r|\n|$))", std::regex::icase);
    return std::regex_search(source, expression);
}
//=============================================================================
static bool
testAllExampleScripts()
{
    const fs::path modulesRoot = fs::path(NELSON_SOURCE_ROOT) / "modules";
    bool ok = true;
    for (const auto& module : fs::directory_iterator(modulesRoot)) {
        const fs::path examples = module.path() / "examples";
        if (!module.is_directory() || !fs::is_directory(examples)) {
            continue;
        }
        for (const auto& entry : fs::recursive_directory_iterator(examples)) {
            if (!entry.is_regular_file() || entry.path().extension() != ".m") {
                continue;
            }
            ok = expect(!containsSessionTermination(readSource(entry.path())),
                     "example terminates the Nelson session: " + entry.path().string())
                && ok;
        }
    }
    return expect(containsSessionTermination("EXIT ();"), "spaced exit call was not detected")
        && expect(
            containsSessionTermination("value = 1; quit;"), "command-style quit was not detected")
        && expect(!containsSessionTermination("application.Quit;"),
            "external object method was mistaken for a Nelson command")
        && expect(!containsSessionTermination("invoke(application, 'Quit');"),
            "external automation method was mistaken for a Nelson command")
        && ok;
}
//=============================================================================
static bool
testFrontendContract()
{
    const fs::path root
        = fs::path(NELSON_SOURCE_ROOT) / "modules" / "webview" / "frontend" / "examples";
    const std::string html = readSource(root / "index.html");
    const std::string css = readSource(root / "examples.css");
    const std::string sharedCss = readSource(root / "gallery-shared.css");
    const std::string script = readSource(root / "examples.js");
    const std::string gallery = readSource(root / "example-gallery.js");
    return expect(html.find("id=\"gallery\"") != std::string::npos, "gallery host is missing")
        && expect(html.find("src=\"webui.js\"") != std::string::npos,
            "gallery does not load the WebUI bridge")
        && expect(html.find("type=\"module\" src=\"examples.js\"") != std::string::npos,
            "gallery page does not load the shared component as a module")
        && expect(css.find("prefers-color-scheme: dark") != std::string::npos,
            "gallery dark theme is missing")
        && expect(html.find("href=\"gallery-shared.css\"") != std::string::npos,
            "gallery does not load the shared styles")
        && expect(sharedCss.find(".example-gallery-card") != std::string::npos,
            "shared gallery card styles are missing")
        && expect(sharedCss.find("overflow-y: auto") != std::string::npos,
            "gallery module rail does not scroll independently")
        && expect(sharedCss.find("scrollbar-gutter: stable") != std::string::npos,
            "gallery module rail does not reserve scrollbar space")
        && expect(sharedCss.find(".example-gallery.compact") != std::string::npos,
            "gallery has no narrow layout")
        && expect(script.find("mountExampleGallery(") != std::string::npos,
            "gallery page does not mount the shared component")
        && expect(script.find("action('open', entry)") != std::string::npos,
            "gallery open action is missing")
        && expect(script.find("action('run', entry)") != std::string::npos,
            "gallery run action is missing")
        && expect(script.find("rpc(refresh ? 'refresh' : 'list')") != std::string::npos,
            "gallery refresh action is missing")
        && expect(script.find("window.webui.call('examplesRpc', payload)") != std::string::npos,
            "gallery does not use the connected WebUI transport")
        && expect(script.find("new URLSearchParams(window.location.search)") != std::string::npos
                && script.find("urlParams.get('channel')") != std::string::npos,
            "gallery does not isolate browser channels by desktop instance")
        && expect(script.find("waitForTransport") != std::string::npos,
            "gallery does not wait for the asynchronous bridge")
        && expect(script.find("expectsNativeBridge") != std::string::npos,
            "gallery falls back to the static fetch path instead of waiting for the desktop bridge")
        && expect(script.find("entry.kind === 'command'") != std::string::npos,
            "gallery does not branch on command entries")
        && expect(gallery.find("textContent") != std::string::npos,
            "gallery does not render manifest text safely")
        && expect(gallery.find("main.scrollTop = 0") != std::string::npos,
            "gallery category selection does not reveal the first matching example")
        && expect(gallery.find("data-kind") == std::string::npos
                || gallery.find("node.dataset.kind = example.kind") != std::string::npos,
            "gallery cards do not expose their kind")
        && expect(gallery.find("example.kind === 'model'") != std::string::npos,
            "gallery does not open models")
        && expect(gallery.find("disabled") == std::string::npos
                && script.find("disabled") == std::string::npos,
            "gallery disables an example action");
}
//=============================================================================
static bool
testGatewayShutdownContract()
{
    const std::string source = readSource(
        fs::path(NELSON_SOURCE_ROOT) / "modules" / "webview" / "builtin" / "cpp" / "Gateway.cpp");
    return expect(source.find("finishWebviewModule") != std::string::npos,
               "webview gateway has no shutdown hook")
        && expect(source.find("CloseAllWebViewers()") != std::string::npos,
            "HTML viewers are not closed during shutdown")
        && expect(source.find("closeExamplesWindow()") != std::string::npos,
            "examples gallery is not closed during shutdown")
        && expect(source.find("webui_clean()") != std::string::npos,
            "WebUI resources are not cleaned during shutdown")
        && expect(source.find("NLS_GATEWAY_REMOVE_EXTENDED") != std::string::npos,
            "webview shutdown hook is not registered");
}
//=============================================================================
static bool
testNativeFocusContract()
{
    const fs::path root = fs::path(NELSON_SOURCE_ROOT) / "modules" / "webview";
    const std::string builtin = readSource(root / "builtin" / "cpp" / "examplesBuiltin.cpp");
    const std::string webui = readSource(root / "src" / "webui" / "src" / "webui.c");
    const std::string appkit
        = readSource(root / "src" / "webui" / "src" / "webview" / "wkwebview.m");
    return expect(builtin.find("webui_focus(state.window)") != std::string::npos,
               "a repeated demo call does not request focus")
        && expect(webui.find("_webui_macos_wv_focus((int)window)") != std::string::npos,
            "WebUI does not forward focus to the macOS WebView")
        && expect(appkit.find("[window deminiaturize:nil]") != std::string::npos,
            "the macOS WebView is not restored before focusing")
        && expect(appkit.find("[window makeKeyAndOrderFront:nil]") != std::string::npos,
            "the macOS WebView is not brought to the front")
        && expect(appkit.find("- (BOOL)acceptsFirstMouse:(NSEvent*)event") != std::string::npos,
            "the macOS WebView consumes the first click while its window is inactive")
        && expect(appkit.find("[[WebUIWebView alloc]") != std::string::npos,
            "the macOS WebView does not use the first-click-aware view");
}
//=============================================================================
int
main()
{
    const fs::path root = fs::path(NELSON_SOURCE_ROOT) / "temp" / "webview_examples_catalog_tests";
    fs::remove_all(root);
    fs::create_directories(root);
    bool ok = testValidManifest(root);
    ok = testInvalidManifest(root, "bad-schema", R"({"schemaVersion":2,"examples":[]})") && ok;
    ok = testInvalidManifest(root, "fractional-schema", R"({"schemaVersion":1.5,"examples":[]})")
        && ok;
    ok = testInvalidManifest(root, "bad-localization",
             R"({"schemaVersion":1,"examples":[{"file":"demo.m","title":{"de_DE":"Demo"},"description":"Demo"}]})")
        && ok;
    ok = testInvalidManifest(root, "missing-description",
             R"({"schemaVersion":1,"examples":[{"file":"demo.m","title":"Demo"}]})")
        && ok;
    ok = testInvalidManifest(root, "bad-tags",
             R"({"schemaVersion":1,"examples":[{"file":"demo.m","title":"Demo","description":"Demo","tags":"not-an-array"}]})")
        && ok;
    ok = testInvalidManifest(root, "bad-requirement",
             R"({"schemaVersion":1,"examples":[{"file":"demo.m","title":"Demo","description":"Demo","requirements":["unknown"]}]})")
        && ok;
    ok = testInvalidManifest(root, "traversal",
             R"({"schemaVersion":1,"examples":[{"file":"../demo.m","title":"A","description":"A"}]})")
        && ok;
    ok = testInvalidManifest(root, "normalized-traversal",
             R"({"schemaVersion":1,"examples":[{"file":"folder/../demo.m","title":"A","description":"A"}]})")
        && ok;
    ok = testInvalidManifest(root, "file-and-command",
             R"json({"schemaVersion":1,"examples":[{"file":"demo.m","command":"morse.practice()","title":"A","description":"A"}]})json")
        && ok;
    ok = testInvalidManifest(root, "neither-file-nor-command",
             R"({"schemaVersion":1,"examples":[{"title":"A","description":"A"}]})")
        && ok;
    ok = testInvalidManifest(root, "empty-command",
             R"({"schemaVersion":1,"examples":[{"command":"","title":"A","description":"A"}]})")
        && ok;
    ok = testCommandManifest(root) && ok;
    ok = testCommandBridge() && ok;
    ok = testBridge(root) && ok;
    ok = testAbsentManifest(root) && ok;
    ok = testResilientManifest(root) && ok;
    ok = testExternalUnknownRequirement(root) && ok;
    ok = testExcludedNflowModule(root) && ok;
    ok = testProviderGroups(root) && ok;
    ok = testModelBridge(root) && ok;
    ok = testEscapingSymlink(root) && ok;
    ok = testRepositoryManifests() && ok;
    ok = testAllExampleScripts() && ok;
    ok = testFrontendContract() && ok;
    ok = testGatewayShutdownContract() && ok;
    ok = testNativeFocusContract() && ok;
    fs::remove_all(root);
    return ok ? 0 : 1;
}
//=============================================================================

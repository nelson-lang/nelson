//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowBootstrapHtml.hpp"
#include "WebUiHelpers.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
std::string
buildNFlowBootstrapHtml(const BootstrapVars& bootstrapVars)
{
    std::string bg = bootstrapVars.darkMode ? "#1e1e1e" : "#ffffff";
    std::string fg = bootstrapVars.darkMode ? "#cccccc" : "#333333";
    std::string themeAttr = bootstrapVars.darkMode ? "dark" : "light";
    std::string themeVal = bootstrapVars.darkMode ? "1" : "0";

    std::string modelUtf = bootstrapVars.modelFilename;
    std::string uiPathUtf = bootstrapVars.uiPath;
    std::string haveCToolChainJson = bootstrapVars.haveCToolChain ? "true" : "false";
    std::string haveRustToolChainJson = bootstrapVars.haveRustToolChain ? "true" : "false";
    std::string bootstrapVarsJson = "{\"modelFilename\":\"" + escapeWebUiJsonString(modelUtf)
        + "\",\"modelName\":\"" + escapeWebUiJsonString(bootstrapVars.modelName)
        + "\",\"uiPath\":\"" + escapeWebUiJsonString(uiPathUtf) + "\",\"locale\":\""
        + escapeWebUiJsonString(bootstrapVars.locale) + "\",\"haveCToolChain\":"
        + haveCToolChainJson + ",\"haveRustToolChain\":" + haveRustToolChainJson + "}";

    std::string seedScript;
    seedScript += "<script>\n";
    seedScript += webUiSetSessionStorageItemScript("nf-theme-dark", themeVal);
    seedScript += webUiSetLocalStorageItemScript("nf-theme-dark", themeVal);
    seedScript += webUiSetSessionStorageItemScript("nf-i18n-locale", bootstrapVars.locale);
    seedScript += webUiSetLocalStorageItemScript("nf-i18n-locale", bootstrapVars.locale);

    seedScript += webUiSetSessionStorageItemScript("nf-model", modelUtf);
    seedScript += webUiSetSessionStorageItemScript("nf-model-name", bootstrapVars.modelName);
    seedScript += webUiSetSessionStorageItemScript("nf-root-path", uiPathUtf);
    seedScript += webUiSetLocalStorageItemScript("nf-root-path", uiPathUtf);
    seedScript += webUiSetSessionStorageItemScript(
        "nf-preferences-path", bootstrapVars.nflowPreferencesPath);
    seedScript += webUiSetLocalStorageItemScript(
        "nf-preferences-path", bootstrapVars.nflowPreferencesPath);
    seedScript += webUiSetSessionStorageItemScript(
        "nf-have-c-toolchain", bootstrapVars.haveCToolChain ? "1" : "0");
    seedScript += webUiSetLocalStorageItemScript(
        "nf-have-c-toolchain", bootstrapVars.haveCToolChain ? "1" : "0");
    seedScript += webUiSetSessionStorageItemScript(
        "nf-have-rust-toolchain", bootstrapVars.haveRustToolChain ? "1" : "0");
    seedScript += webUiSetLocalStorageItemScript(
        "nf-have-rust-toolchain", bootstrapVars.haveRustToolChain ? "1" : "0");
    seedScript += "try{window.__nf_bootstrap = " + bootstrapVarsJson + ";}catch(_){}\n";
    seedScript += "</script>\n";

    std::string bootstrapHtml;
    bootstrapHtml += "<!doctype html>\n";
    bootstrapHtml += "<html data-theme=\"" + themeAttr + "\" lang=\"" + bootstrapVars.locale
        + "\" style=\"background:" + bg + ";color:" + fg + "\">\n";
    bootstrapHtml += "<head>\n";
    bootstrapHtml += "  <meta charset=\"utf-8\">\n";
    bootstrapHtml += "  <title>NFlow</title>\n";
    bootstrapHtml += seedScript;
    bootstrapHtml += "  <script>location.replace('./nflow.html');</script>\n";
    bootstrapHtml += "</head>\n";
    bootstrapHtml += "<body></body>\n";
    bootstrapHtml += "</html>\n";

    return bootstrapHtml;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================

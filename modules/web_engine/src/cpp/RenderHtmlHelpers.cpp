//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RenderHtmlHelpers.hpp"
#include <algorithm>
#include <cctype>
#include <cstdlib>
//=============================================================================
namespace {
//=============================================================================
std::string
percentEncodeDataUri(const std::string& value)
{
    static constexpr char HEX[] = "0123456789ABCDEF";
    std::string encoded;
    encoded.reserve(value.size() * 3);
    for (unsigned char c : value) {
        if (std::isalnum(c) || c == '-' || c == '_' || c == '.' || c == '~') {
            encoded += static_cast<char>(c);
        } else {
            encoded += '%';
            encoded += HEX[(c >> 4) & 0x0F];
            encoded += HEX[c & 0x0F];
        }
    }
    return encoded;
}
//=============================================================================
std::string
extractHtmlAttribute(const std::string& html, const std::string& name)
{
    const std::string marker = name + "=\"";
    const size_t start = html.find(marker);
    if (start == std::string::npos) {
        return "";
    }
    const size_t valueStart = start + marker.size();
    const size_t valueEnd = html.find('"', valueStart);
    if (valueEnd == std::string::npos) {
        return "";
    }
    return html.substr(valueStart, valueEnd - valueStart);
}
//=============================================================================
int
mathDimensionToPixels(const std::string& value)
{
    if (value.empty()) {
        return 0;
    }
    char* end = nullptr;
    const double number = std::strtod(value.c_str(), &end);
    if (end == value.c_str() || number <= 0) {
        return 0;
    }
    double pixels = number;
    const std::string unit(end);
    if (unit.rfind("ex", 0) == 0) {
        pixels = number * 8.0;
    } else if (unit.rfind("em", 0) == 0) {
        pixels = number * 16.0;
    }
    if (pixels < 1.0) {
        return 1;
    }
    if (pixels > 2048.0) {
        return 2048;
    }
    return static_cast<int>(pixels + 0.5);
}
//=============================================================================
std::string
imageSizeAttributesFromSvg(const std::string& svg)
{
    const int width = mathDimensionToPixels(extractHtmlAttribute(svg, "width"));
    const int height = mathDimensionToPixels(extractHtmlAttribute(svg, "height"));
    if (width <= 0 || height <= 0) {
        return "";
    }
    return " width=\"" + std::to_string(width) + "\" height=\"" + std::to_string(height) + "\"";
}
//=============================================================================
} // namespace
//=============================================================================
namespace Nelson::WebviewRender {
//=============================================================================
std::string
fileUrlFromPath(const std::filesystem::path& path)
{
    std::string url = path.generic_string();
    std::string encoded;
    encoded.reserve(url.size());
    for (char c : url) {
        switch (c) {
        case '%':
            encoded += "%25";
            break;
        case ' ':
            encoded += "%20";
            break;
        case '#':
            encoded += "%23";
            break;
        default:
            encoded += c;
            break;
        }
    }
    return "file:///" + encoded;
}
//=============================================================================
void
replaceMathJaxContainersWithImages(std::string& root)
{
    const std::string endMarker = "</mjx-container>";
    size_t searchPosition = 0;
    while (true) {
        const size_t containerStart = root.find("<mjx-container", searchPosition);
        if (containerStart == std::string::npos) {
            return;
        }
        const size_t openEnd = root.find('>', containerStart);
        const size_t containerEnd = root.find(endMarker, openEnd);
        if (openEnd == std::string::npos || containerEnd == std::string::npos) {
            return;
        }
        const size_t svgStart = root.find("<svg", openEnd);
        const size_t svgClose = root.find("</svg>", svgStart);
        if (svgStart == std::string::npos || svgClose == std::string::npos
            || svgClose > containerEnd) {
            searchPosition = openEnd + 1;
            continue;
        }
        const size_t svgEnd = svgClose + 6;
        const std::string svg = root.substr(svgStart, svgEnd - svgStart);
        const std::string img = "<img" + imageSizeAttributesFromSvg(svg)
            + " src=\"data:image/svg+xml;charset=utf-8," + percentEncodeDataUri(svg)
            + "\" alt=\"\">";
        root.replace(containerStart, containerEnd + endMarker.size() - containerStart, img);
        searchPosition = containerStart + img.size();
    }
}
//=============================================================================
bool
extractRenderedRoot(const std::string& dom, std::string& root)
{
    const std::string marker = "<div id=\"nelson-markdowndisp-root\">";
    const size_t start = dom.find(marker);
    if (start == std::string::npos) {
        return false;
    }
    std::string lowerDom = dom;
    std::transform(lowerDom.begin(), lowerDom.end(), lowerDom.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    const size_t contentStart = start + marker.size();
    size_t position = contentStart;
    int depth = 1;
    while (position < lowerDom.size()) {
        const size_t nextOpen = lowerDom.find("<div", position);
        const size_t nextClose = lowerDom.find("</div>", position);
        if (nextClose == std::string::npos) {
            return false;
        }
        if (nextOpen != std::string::npos && nextOpen < nextClose) {
            ++depth;
            position = nextOpen + 4;
            continue;
        }
        --depth;
        if (depth == 0) {
            root = dom.substr(contentStart, nextClose - contentStart);
            return true;
        }
        position = nextClose + 6;
    }
    return false;
}
//=============================================================================
} // namespace Nelson::WebviewRender
//=============================================================================

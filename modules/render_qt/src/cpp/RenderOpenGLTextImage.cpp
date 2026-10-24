//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#if defined(__APPLE__) && !defined(GL_SILENCE_DEPRECATION)
#define GL_SILENCE_DEPRECATION
#endif
#if defined(_WIN32) && !defined(NOMINMAX)
#define NOMINMAX
#endif
#include <algorithm>
#include <cmath>
#include <limits>
#include <sstream>
#include <unordered_map>
#include <QtGui/QFontMetrics>
#include <QtGui/QOpenGLContext>
#include <QtGui/QPainter>
#if defined(_WIN32)
#include <windows.h>
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#endif
#if defined(__APPLE__)
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "RenderOpenGL.hpp"
#include "RenderQtHelpers.hpp"
#include "QStringConverter.hpp"
//=============================================================================
struct TextImageKey
{
    std::wstring text;
    std::wstring family;
    int pixelSize = 0;
    int pointSize = 0;
    int weight = 0;
    bool italic = false;
    bool underline = false;
    bool antialias = true;
    int red = 0;
    int green = 0;
    int blue = 0;
    int rotation = 0;

    bool
    operator==(const TextImageKey& other) const
    {
        return text == other.text && family == other.family && pixelSize == other.pixelSize
            && pointSize == other.pointSize && weight == other.weight && italic == other.italic
            && underline == other.underline && antialias == other.antialias && red == other.red
            && green == other.green && blue == other.blue && rotation == other.rotation;
    }
};
//=============================================================================
struct TextImageKeyHash
{
    std::size_t
    operator()(const TextImageKey& key) const
    {
        std::size_t seed = std::hash<std::wstring> {}(key.text);
        auto combine = [&seed](std::size_t value) {
            seed ^= value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        };
        combine(std::hash<std::wstring> {}(key.family));
        combine(std::hash<int> {}(key.pixelSize));
        combine(std::hash<int> {}(key.pointSize));
        combine(std::hash<int> {}(key.weight));
        combine(std::hash<bool> {}(key.italic));
        combine(std::hash<bool> {}(key.underline));
        combine(std::hash<bool> {}(key.antialias));
        combine(std::hash<int> {}(key.red));
        combine(std::hash<int> {}(key.green));
        combine(std::hash<int> {}(key.blue));
        combine(std::hash<int> {}(key.rotation));
        return seed;
    }
};
//=============================================================================
struct TextImageEntry
{
    QImage image;
    int textWidth = 0;
    int textHeight = 0;
    double baselineX = 0.;
    double baselineY = 0.;
    int descent = 0;
};
//=============================================================================
static std::unordered_map<TextImageKey, TextImageEntry, TextImageKeyHash>&
textImageCache()
{
    static std::unordered_map<TextImageKey, TextImageEntry, TextImageKeyHash> cache;
    return cache;
}
//=============================================================================
static TextImageKey
makeTextImageKey(const std::wstring& text, const FontSpec& font, const std::vector<double>& color,
    double rotation)
{
    TextImageKey key;
    key.text = text;
    key.family = font.family;
    key.pixelSize = static_cast<int>(std::lround(font.pixelSize * 100.0));
    key.pointSize = static_cast<int>(std::lround(font.pointSizeF * 100.0));
    key.weight = font.weight;
    key.italic = font.italic;
    key.underline = font.underline;
    key.antialias = font.antialias;
    key.red = std::clamp(static_cast<int>(std::lround(color[0] * 255.0)), 0, 255);
    key.green = std::clamp(static_cast<int>(std::lround(color[1] * 255.0)), 0, 255);
    key.blue = std::clamp(static_cast<int>(std::lround(color[2] * 255.0)), 0, 255);
    key.rotation = static_cast<int>(std::lround(rotation * 100.0));
    return key;
}
//=============================================================================
static std::wstring
textTextureCacheKey(const TextImageKey& key)
{
    std::wstringstream stream;
    stream << key.text << L'\x1f' << key.family << L'\x1f' << key.pixelSize << L'\x1f'
           << key.pointSize << L'\x1f' << key.weight << L'\x1f' << key.italic << L'\x1f'
           << key.underline << L'\x1f' << key.antialias << L'\x1f' << key.red << L'\x1f'
           << key.green << L'\x1f' << key.blue << L'\x1f' << key.rotation;
    return stream.str();
}
//=============================================================================
static GLuint
uploadRgbaTexture(const QImage& image, GLint minFilter, GLint magFilter)
{
    GLuint texture = 0;
    glGenTextures(1, &texture);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, minFilter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, magFilter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image.width(), image.height(), 0, GL_RGBA,
        GL_UNSIGNED_BYTE, image.bits());
    return texture;
}
//=============================================================================
void
RenderOpenGL::clearImageTextureCache()
{
    if (QOpenGLContext::currentContext() == imageTextureContext.data()) {
        for (const auto& item : imageTextureCache) {
            for (const auto& slot : item.second.textures) {
                GLuint texture = static_cast<GLuint>(slot.texture);
                if (texture != 0) {
                    glDeleteTextures(1, &texture);
                }
            }
        }
    }
    imageTextureCache.clear();
    imageTextureContext.clear();
    imageTextureUse = 0;
}
//=============================================================================
unsigned int
RenderOpenGL::imageTexture(const RGBAImageView& image)
{
    const int stride = image.bytesPerLine > 0 ? image.bytesPerLine : image.width * 4;
    if (image.resourceKey == 0 || image.pixels == nullptr || image.width <= 0 || image.height <= 0
        || stride != image.width * 4) {
        return 0;
    }

    QOpenGLContext* context = QOpenGLContext::currentContext();
    if (context == nullptr) {
        return 0;
    }
    if (context != imageTextureContext.data()) {
        imageTextureCache.clear();
        imageTextureContext = context;
        imageTextureUse = 0;
    }

    auto found = imageTextureCache.find(image.resourceKey);

    bool newEntry = found == imageTextureCache.end();
    if (newEntry) {
        constexpr size_t maximumCachedImages = 64;
        if (imageTextureCache.size() >= maximumCachedImages) {
            auto oldest = std::min_element(imageTextureCache.begin(), imageTextureCache.end(),
                [](const auto& left, const auto& right) {
                    return left.second.lastUse < right.second.lastUse;
                });
            for (const auto& slot : oldest->second.textures) {
                GLuint oldestTexture = static_cast<GLuint>(slot.texture);
                if (oldestTexture != 0) {
                    glDeleteTextures(1, &oldestTexture);
                }
            }
            imageTextureCache.erase(oldest);
        }
        found = imageTextureCache.emplace(image.resourceKey, ImageTextureCacheEntry {}).first;
    } else if (found->second.revision != image.contentRevision) {
        found->second.activeSlot = (found->second.activeSlot + 1) % found->second.textures.size();
    }

    ImageTextureSlot& slot = found->second.textures[found->second.activeSlot];
    bool newTexture = slot.texture == 0;
    if (newTexture) {
        GLuint texture = 0;
        glGenTextures(1, &texture);
        if (texture == 0) {
            if (newEntry) {
                imageTextureCache.erase(found);
            }
            return 0;
        }
        slot.texture = texture;
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    } else {
        glEnable(GL_TEXTURE_2D);
        glBindTexture(GL_TEXTURE_2D, static_cast<GLuint>(slot.texture));
    }

    const bool contentChanged = found->second.revision != image.contentRevision;
    const bool sizeChanged = slot.width != image.width || slot.height != image.height;
    if (newTexture || sizeChanged) {
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image.width, image.height, 0, GL_RGBA,
            GL_UNSIGNED_BYTE, image.pixels);
        slot.width = image.width;
        slot.height = image.height;
    } else if (contentChanged) {
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, image.width, image.height, GL_RGBA,
            GL_UNSIGNED_BYTE, image.pixels);
    }
    found->second.revision = image.contentRevision;
    found->second.lastUse = ++imageTextureUse;
    return slot.texture;
}
//=============================================================================
static void
drawTexturedQuad2D(double left, double bottom, double right, double top, const QImage& image,
    GLint minFilter, GLint magFilter, const double restoreColor[4])
{
    if (image.isNull()) {
        return;
    }
    GLuint texture = uploadRgbaTexture(image, minFilter, magFilter);
    const GLdouble vertices[] = { left, bottom, 0, right, bottom, 0, right, top, 0, left, top, 0 };
    const GLdouble texCoords[] = { 0, 0, 1, 0, 1, 1, 0, 1 };
#if defined(GL_POLYGON_STIPPLE)
    glDisable(GL_POLYGON_STIPPLE);
#endif
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_LIGHTING);
    glColor4d(1, 1, 1, 1);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glVertexPointer(3, GL_DOUBLE, 0, vertices);
    glTexCoordPointer(2, GL_DOUBLE, 0, texCoords);
    glDrawArrays(GL_QUADS, 0, 4);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);

    glBindTexture(GL_TEXTURE_2D, 0);
    glDeleteTextures(1, &texture);
    glDisable(GL_TEXTURE_2D);
    glColor4dv(restoreColor);
}
//=============================================================================
static void
drawTexturedQuadTexture2D(double left, double bottom, double right, double top, GLuint texture,
    const double restoreColor[4])
{
    if (texture == 0) {
        return;
    }
    const GLdouble vertices[] = { left, bottom, 0, right, bottom, 0, right, top, 0, left, top, 0 };
    const GLdouble texCoords[] = { 0, 0, 1, 0, 1, 1, 0, 1 };
#if defined(GL_POLYGON_STIPPLE)
    glDisable(GL_POLYGON_STIPPLE);
#endif
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_LIGHTING);
    glColor4d(1, 1, 1, 1);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glVertexPointer(3, GL_DOUBLE, 0, vertices);
    glTexCoordPointer(2, GL_DOUBLE, 0, texCoords);
    glDrawArrays(GL_QUADS, 0, 4);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);

    glBindTexture(GL_TEXTURE_2D, 0);
    glDisable(GL_TEXTURE_2D);
    glColor4dv(restoreColor);
}
//=============================================================================
static void
drawTexturedQuadTexture3D(double left, double bottom, double right, double top, double u1,
    double v1, double u2, double v2, GLuint texture, bool opaque, const double restoreColor[4])
{
    if (texture == 0) {
        return;
    }
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture);
    const GLdouble vertices[] = { left, bottom, 0, right, bottom, 0, right, top, 0, left, top, 0 };
    const GLdouble texCoords[] = { u1, v2, u2, v2, u2, v1, u1, v1 };
#if defined(GL_POLYGON_STIPPLE)
    glDisable(GL_POLYGON_STIPPLE);
#endif
    if (opaque) {
        glDisable(GL_BLEND);
    } else {
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }
    glDisable(GL_LIGHTING);
    glColor4d(1, 1, 1, 1);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glVertexPointer(3, GL_DOUBLE, 0, vertices);
    glTexCoordPointer(2, GL_DOUBLE, 0, texCoords);
    glDrawArrays(GL_QUADS, 0, 4);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);

    glBindTexture(GL_TEXTURE_2D, 0);
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glColor4dv(restoreColor);
}
//=============================================================================
static void
drawTexturedQuad3D(double left, double bottom, double right, double top, double u1, double v1,
    double u2, double v2, const QImage& image, const double restoreColor[4])
{
    if (image.isNull()) {
        return;
    }
    GLuint texture = uploadRgbaTexture(image, GL_NEAREST, GL_NEAREST);
    drawTexturedQuadTexture3D(
        left, bottom, right, top, u1, v1, u2, v2, texture, false, restoreColor);
    glDeleteTextures(1, &texture);
}
//=============================================================================
static QImage
cropAndOrientImage(
    const QImage& image, double x1, double y1, double x2, double y2, bool mirrorX, bool mirrorY)
{
    int sx = std::clamp(static_cast<int>(std::floor(std::min(x1, x2))), 0, image.width());
    int sy = std::clamp(static_cast<int>(std::floor(std::min(y1, y2))), 0, image.height());
    int ex = std::clamp(static_cast<int>(std::ceil(std::max(x1, x2))), 0, image.width());
    int ey = std::clamp(static_cast<int>(std::ceil(std::max(y1, y2))), 0, image.height());
    int sw = ex - sx;
    int sh = ey - sy;
    if (sw <= 0 || sh <= 0) {
        return {};
    }
    QImage cropped = image.copy(sx, sy, sw, sh);
    Qt::Orientations orientations;
    if (mirrorX) {
        orientations |= Qt::Horizontal;
    }
    if (mirrorY) {
        orientations |= Qt::Vertical;
    }
    return orientations ? Nelson::flipImage(cropped, orientations) : cropped;
}
//=============================================================================
std::vector<std::wstring>
RenderOpenGL::splitEoLText(const std::wstring& text)
{
    wchar_t delim = L'\n';
    std::wstring line;
    std::vector<std::wstring> vec;
    std::wstringstream ss(text);
    while (std::getline(ss, line, delim)) {
        vec.push_back(line);
    }
    return vec;
}
//=============================================================================
QRect
RenderOpenGL::boundingRectMultiLine(const std::wstring& text, QFont fnt, size_t& nbLines)
{
    QFontMetrics fm(fnt);
    std::vector<std::wstring> lines = splitEoLText(text);
    nbLines = lines.size();
    if (nbLines < 2) {
        return fm.boundingRect(Nelson::wstringToQString(text));
    }
    // QFontMetrics::boundingRect(QString) measures a single line: it counts a
    // newline as one more (unprintable) glyph instead of starting a new line.
    // The block is as wide as its widest line and grows by one line advance
    // per extra line, the last line keeping its own ink box.
    QRect sze = fm.boundingRect(Nelson::wstringToQString(lines.back()));
    int widest = 0;
    for (const std::wstring& line : lines) {
        widest = std::max(widest, fm.boundingRect(Nelson::wstringToQString(line)).width());
    }
    sze.setWidth(widest);
    sze.setHeight(sze.height() + static_cast<int>(nbLines - 1) * fm.lineSpacing());
    sze.moveTop(sze.top() - static_cast<int>(nbLines - 1) * fm.lineSpacing());
    return sze;
}
//=============================================================================
void
RenderOpenGL::measureText(std::wstring txt, const FontSpec& fntSpec, AlignmentFlag xflag,
    AlignmentFlag yflag, int& width, int& height, int& xoffset, int& yoffset)
{
    QFont fnt = Nelson::qFontFromSpec(fntSpec);
    size_t nbLines;
    QRect sze = boundingRectMultiLine(txt, fnt, nbLines);
    width = sze.width();
    height = sze.height();
    yoffset = -height;
    xoffset = 0;
    if (xflag == Mean) {
        xoffset -= width / 2;
    } else if (xflag == Max) {
        xoffset -= width;
    }
    if (yflag == Mean) {
        yoffset += height / 2;
    } else if (yflag == Min) {
        yoffset += height;
    }
}
//=============================================================================
void
RenderOpenGL::putText(double x, double y, std::wstring txt, std::vector<double> col,
    AlignmentFlag xflag, AlignmentFlag yflag, const FontSpec& fntSpec, double rotation)
{
    TextImageKey key = makeTextImageKey(txt, fntSpec, col, rotation);
    std::wstring textureKey = textTextureCacheKey(key);
    auto& cache = textImageCache();
    auto found = cache.find(key);
    if (found == cache.end()) {
        QFont fnt = Nelson::qFontFromSpec(fntSpec);
        size_t nbLines;
        QRect sze = boundingRectMultiLine(txt, fnt, nbLines);
        QFontMetrics fm(fnt);
        int width = (sze.width() > 1) ? sze.width() : 1;
        int height = (sze.height() > 1) ? sze.height() : 1;
        int margin = 4;
        if (rotation != 0) {
            margin += width + height;
        }
        QImage image(width + 2 * margin, height + 2 * margin, QImage::Format_ARGB32);
        image.fill(Qt::transparent);
        QPainter painter(&image);
        painter.setRenderHint(QPainter::TextAntialiasing);
        painter.setFont(fnt);
        painter.setPen(QColor(key.red, key.green, key.blue));
        double baselineX = margin - sze.left();
        double baselineY = margin - sze.top();
        if (rotation != 0) {
            baselineX = image.width() / 2.0;
            baselineY = image.height() / 2.0;
        }
        painter.translate(baselineX, baselineY);
        painter.rotate(-rotation);
        if (nbLines > 1) {
            // The last line sits on the baseline the single-line case draws
            // at; earlier lines stack above it by one line advance each.
            const std::vector<std::wstring> lines = splitEoLText(txt);
            for (size_t k = 0; k < lines.size(); ++k) {
                const int baseline = -static_cast<int>(lines.size() - 1 - k) * fm.lineSpacing();
                painter.drawText(0, baseline, Nelson::wstringToQString(lines[k]));
            }
        } else {
            painter.drawText(0, 0, Nelson::wstringToQString(txt));
        }
        painter.end();

        TextImageEntry entry;
        entry.image
            = Nelson::flipImage(image.convertToFormat(QImage::Format_RGBA8888), Qt::Vertical);
        entry.textWidth = width;
        entry.textHeight = height;
        entry.baselineX = baselineX;
        entry.baselineY = baselineY;
        entry.descent = fm.descent();
        if (cache.size() > 512) {
            cache.clear();
        }
        found = cache.emplace(std::move(key), std::move(entry)).first;
    }
    const TextImageEntry& textImage = found->second;

    double xdelta = 0;
    double ydelta = 0;
    if (xflag == Mean) {
        xdelta = -textImage.textWidth / 2.0;
    } else if (xflag == Max) {
        xdelta = -textImage.textWidth;
    }
    if (yflag == Mean) {
        ydelta = -textImage.textHeight / 2.0;
    } else if (yflag == Max) {
        ydelta = -textImage.textHeight;
    }
    ydelta += textImage.descent;
    double costhet = cos(rotation * M_PI / 180.0);
    double sinthet = sin(rotation * M_PI / 180.0);
    double xpos = x + xdelta * costhet - ydelta * sinthet;
    double ypos = y + xdelta * sinthet + ydelta * costhet;
    QPointF pixel = map(xpos, ypos, 0);

    bool needsDirectDraw = !inDirect;
    if (needsDirectDraw) {
        setupDirectDraw();
    }
    double left = pixel.x() - textImage.baselineX;
    double bottom = pixel.y() - (textImage.image.height() - textImage.baselineY);
    auto textureFound = textTextureCache.find(textureKey);
    if (textureFound != textTextureCache.end()
        && (textureFound->second.width != textImage.image.width()
            || textureFound->second.height != textImage.image.height()
            || !glIsTexture(static_cast<GLuint>(textureFound->second.texture)))) {
        GLuint texture = static_cast<GLuint>(textureFound->second.texture);
        if (texture != 0 && glIsTexture(texture)) {
            glDeleteTextures(1, &texture);
        }
        textTextureCache.erase(textureFound);
        textureFound = textTextureCache.end();
    }
    if (textureFound == textTextureCache.end()) {
        if (textTextureCache.size() > 512) {
            clearTextTextureCache();
        }
        GLuint texture = uploadRgbaTexture(textImage.image, GL_LINEAR, GL_LINEAR);
        textureFound = textTextureCache
                           .emplace(std::move(textureKey),
                               TextureCacheEntry {
                                   texture, textImage.image.width(), textImage.image.height() })
                           .first;
    }
    drawTexturedQuadTexture2D(left, bottom, left + textImage.image.width(),
        bottom + textImage.image.height(), static_cast<GLuint>(textureFound->second.texture),
        currentColor);
    if (needsDirectDraw) {
        releaseDirectDraw();
    }
}
//=============================================================================
void
RenderOpenGL::drawImage(int x1, int y1, const RGBAImageView& picView)
{
    bool needsDirectDraw = !inDirect;
    if (needsDirectDraw) {
        setupDirectDraw();
    }
    GLuint texture = static_cast<GLuint>(imageTexture(picView));
    if (texture != 0) {
        drawTexturedQuadTexture3D(x1, y1, x1 + picView.width, y1 + picView.height, 0.0, 0.0, 1.0,
            1.0, texture, picView.opaque, currentColor);
    } else {
        QImage pic = Nelson::qImageFromView(picView);
        QImage glImage
            = Nelson::flipImage(pic.convertToFormat(QImage::Format_RGBA8888), Qt::Vertical);
        drawTexturedQuad2D(x1, y1, x1 + glImage.width(), y1 + glImage.height(), glImage, GL_NEAREST,
            GL_NEAREST, currentColor);
    }
    if (needsDirectDraw) {
        releaseDirectDraw();
    }
}
//=============================================================================
void
RenderOpenGL::drawImage(double x1, double y1, double, double, const RGBAImageView& pic)
{
    QPointF pixel = map(x1, y1, 0);
    drawImage(static_cast<int>(pixel.x()), static_cast<int>(pixel.y()), pic);
}
//=============================================================================
void
RenderOpenGL::drawImage(const std::vector<double>& xp, const std::vector<double>& yp,
    const std::vector<double>& xLim, const std::vector<double>& yLim, bool xFlip, bool yFlip,
    const RGBAImageView& picView)
{
    if (xp.size() < 2 || yp.size() < 2 || xLim.size() < 2 || yLim.size() < 2
        || picView.pixels == nullptr || picView.width <= 0 || picView.height <= 0) {
        return;
    }
    double dx
        = (picView.width > 1) ? ((xp[1] - xp[0]) / static_cast<double>(picView.width - 1)) : 1.0;
    double dy
        = (picView.height > 1) ? ((yp[1] - yp[0]) / static_cast<double>(picView.height - 1)) : 1.0;
    double dataX1 = xp[0] - dx / 2.0;
    double dataY1 = yp[0] - dy / 2.0;
    double dataX2 = xp[1] + dx / 2.0;
    double dataY2 = yp[1] + dy / 2.0;

    double limX1 = xLim[0];
    double limY1 = yLim[0];
    double limX2 = xLim[1];
    double limY2 = yLim[1];

    // A span given the other way round (XData or YData descending) mirrors the
    // image along that axis. Clip against the ordered interval: clipping against
    // the raw one collapses a descending span to a point and drops the image.
    bool xDescending = (xp[1] < xp[0]);
    bool yDescending = (yp[1] < yp[0]);
    double dataXLo = std::min(dataX1, dataX2);
    double dataXHi = std::max(dataX1, dataX2);
    double dataYLo = std::min(dataY1, dataY2);
    double dataYHi = std::max(dataY1, dataY2);

    double visX1 = std::clamp(limX1, dataXLo, dataXHi);
    double visX2 = std::clamp(limX2, dataXLo, dataXHi);
    double visY1 = std::clamp(limY1, dataYLo, dataYHi);
    double visY2 = std::clamp(limY2, dataYLo, dataYHi);

    double dataWidth = dataX2 - dataX1;
    double dataHeight = dataY2 - dataY1;
    if (std::fabs(dataWidth) < std::numeric_limits<double>::epsilon()
        || std::fabs(dataHeight) < std::numeric_limits<double>::epsilon()) {
        return;
    }

    double imgX1 = (visX1 - dataX1) * picView.width / dataWidth;
    double imgX2 = (visX2 - dataX1) * picView.width / dataWidth;
    double imgY1 = (visY1 - dataY1) * picView.height / dataHeight;
    double imgY2 = (visY2 - dataY1) * picView.height / dataHeight;

    if (yFlip) {
        visY1 = limY1 + limY2 - visY1;
        visY2 = limY1 + limY2 - visY2;
    }

    if (xFlip) {
        visX1 = limX1 + limX2 - visX1;
        visX2 = limX1 + limX2 - visX2;
    }

    double left = std::min(visX1, visX2);
    double right = std::max(visX1, visX2);
    double bottom = std::min(visY1, visY2);
    double top = std::max(visY1, visY2);
    if (std::fabs(right - left) < std::numeric_limits<double>::epsilon()
        || std::fabs(top - bottom) < std::numeric_limits<double>::epsilon()) {
        return;
    }

    int sx = std::clamp(static_cast<int>(std::floor(std::min(imgX1, imgX2))), 0, picView.width);
    int sy = std::clamp(static_cast<int>(std::floor(std::min(imgY1, imgY2))), 0, picView.height);
    int ex = std::clamp(static_cast<int>(std::ceil(std::max(imgX1, imgX2))), 0, picView.width);
    int ey = std::clamp(static_cast<int>(std::ceil(std::max(imgY1, imgY2))), 0, picView.height);
    if (ex <= sx || ey <= sy) {
        return;
    }

    GLuint texture = static_cast<GLuint>(imageTexture(picView));
    if (texture != 0) {
        double u1 = static_cast<double>(sx) / picView.width;
        double u2 = static_cast<double>(ex) / picView.width;
        double v1 = static_cast<double>(sy) / picView.height;
        double v2 = static_cast<double>(ey) / picView.height;
        if (xFlip != xDescending) {
            std::swap(u1, u2);
        }
        if ((!yFlip) != yDescending) {
            std::swap(v1, v2);
        }
        drawTexturedQuadTexture3D(
            left, bottom, right, top, u1, v1, u2, v2, texture, picView.opaque, currentColor);
        return;
    }

    QImage pic = Nelson::qImageFromView(picView);
    QImage oriented = cropAndOrientImage(
        pic, imgX1, imgY1, imgX2, imgY2, xFlip != xDescending, (!yFlip) != yDescending);
    QImage glImage = oriented.convertToFormat(QImage::Format_RGBA8888);
    drawTexturedQuad3D(left, bottom, right, top, 0.0, 0.0, 1.0, 1.0, glImage, currentColor);
}
//=============================================================================

//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
// Qt <-> Qt-free render types. The render interface
// carries FontSpec / RGBAImageView; Qt-side painters and backends convert at
// the boundary with these helpers. Header-only on purpose: usable from any
// Qt-linked TU without build wiring.
//=============================================================================
#include <QtGui/QFont>
#include <QtGui/QImage>
#include "RenderInterface.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline FontSpec
fontSpecFromQFont(const QFont& font)
{
    FontSpec spec;
    spec.family = QStringTowstring(font.family());
    spec.pointSizeF = font.pointSizeF();
    spec.pixelSize = font.pixelSize() > 0 ? font.pixelSize() : 0.0;
    spec.setWeight(static_cast<int>(font.weight())); // Qt6 weights are CSS-scaled
    spec.italic = font.italic();
    spec.underline = font.underline();
    spec.antialias = font.styleStrategy() != QFont::NoAntialias;
    if (font.stretch() > 0) {
        spec.stretch = font.stretch() / 100.0;
    }
    return spec;
}
//=============================================================================
inline QFont
qFontFromSpec(const FontSpec& spec)
{
    QFont font(wstringToQString(spec.family));
    if (spec.pixelSize > 0) {
        font.setPixelSize(static_cast<int>(spec.pixelSize));
    } else if (spec.pointSizeF > 0) {
        font.setPointSizeF(spec.pointSizeF);
    }
    if (spec.weight != 400) {
        font.setWeight(static_cast<QFont::Weight>(spec.weight));
    } else {
        font.setBold(spec.bold);
    }
    font.setItalic(spec.italic);
    font.setUnderline(spec.underline);
    font.setStyleStrategy(spec.antialias ? QFont::PreferAntialias : QFont::NoAntialias);
    if (spec.stretch > 0 && spec.stretch != 1.0) {
        font.setStretch(static_cast<int>(spec.stretch * 100.0));
    }
    return font;
}
//=============================================================================
// Owning RGBA conversion of a QImage: keeps the converted pixels alive while
// the view is in use (the view is only valid for this object's lifetime).
class QImageRgba
{
public:
    explicit QImageRgba(const QImage& image)
        : m_image(image.convertToFormat(QImage::Format_RGBA8888))
    {
    }
    RGBAImageView
    view() const
    {
        RGBAImageView v;
        v.width = m_image.width();
        v.height = m_image.height();
        v.bytesPerLine = static_cast<int>(m_image.bytesPerLine());
        v.pixels = m_image.constBits();
        return v;
    }

private:
    QImage m_image;
};
//=============================================================================
// Backend-side: wrap an incoming view into a QImage WITHOUT copying. The view
// points at storage the caller keeps alive for the whole (synchronous) draw,
// so a read-only QImage that references it directly is safe and avoids a full
// per-frame deep copy of the pixel buffer. Callers that need to keep the image
// past the draw, or to mutate it, must copy() it themselves.
inline QImage
qImageFromView(const RGBAImageView& view)
{
    if (view.pixels == nullptr || view.width <= 0 || view.height <= 0) {
        return {};
    }
    const int stride = view.bytesPerLine > 0 ? view.bytesPerLine : view.width * 4;
    return QImage(view.pixels, view.width, view.height, stride, QImage::Format_RGBA8888);
}
//=============================================================================
// QImage::flipped() was introduced in Qt 6.9. Fall back to the (now deprecated)
// QImage::mirrored() on older Qt (e.g. 6.4, shipped by Ubuntu 24.04).
inline QImage
flipImage(const QImage& image, Qt::Orientations orientations)
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 9, 0)
    return image.flipped(orientations);
#else
    return image.mirrored(
        orientations.testFlag(Qt::Horizontal), orientations.testFlag(Qt::Vertical));
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================

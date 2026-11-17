---
name: nelson-usage-image-processing
audience: user
modules: image_processing, graphics_io
description: Read, transform, filter and analyze images in Nelson - resize/rotate, color conversion, filtering, thresholding and edges. Use when the user loads, processes, or saves an image.
triggers: image, images, photo, pixel, imread, imwrite, imshow, redimensionner, resize, rotation, rotate, niveaux de gris, grayscale, rgb2gray, filtre, filter, imfilter, contraste, contrast, seuil, threshold, binariser, binarize, contour, edge, hsv, bruit, noise
functions: imread, imwrite, imshow, imresize, imrotate, rgb2gray, rgb2hsv, im2double, im2uint8, imfilter, fspecial, imadjust, imhist, imbinarize, graythresh, edge, imcrop, imcomplement, imnoise
---

# Image Processing

Load an image, transform or filter it, and save the result, keeping the pixel type and range consistent.

## When to use
- Reading/writing images, resizing, rotating, cropping.
- Color conversion, filtering, contrast/threshold, edges.

## When NOT to use
- Plotting numeric curves (use the plotting guide).

## Key functions
I/O: `imread`, `imwrite`, `imshow`. Geometry: `imresize`, `imrotate`, `imcrop`. Color/type: `rgb2gray`, `rgb2hsv`, `im2double`, `im2uint8`, `imcomplement`. Filter/enhance: `imfilter`, `fspecial`, `imadjust`, `imhist`, `imnoise`. Threshold/edges: `imbinarize`, `graythresh`, `edge`. (Signatures in the Verified Nelson API block.)

## Idioms & workflow
1. Know the pixel type: `imread` returns `uint8` (0-255); convert to `double` (0-1) with `im2double` before arithmetic, back with `im2uint8` before saving.
2. Convert to grayscale with `rgb2gray` before single-channel operations (threshold, edges).
3. Build a filter kernel with `fspecial` and apply it with `imfilter`.
4. Threshold via `imbinarize(img, graythresh(img))` for an automatic level.
5. Keep the dimensions in mind: a color image is H x W x 3; indexing drops/selects channels.

## Gotchas
- Mixing `uint8` and `double` ranges gives a black or saturated image; convert consistently.
- `imshow` of a `double` image expects values in 0-1; scale first.
- `imfilter` edge handling changes borders; pick the padding option deliberately.

## Done when
The image is read/processed/written with pixel type and range consistent end to end, and the result matches what the user asked (resized, filtered, thresholded, etc.).

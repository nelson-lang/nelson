param(
  [string]$ReferenceExe = $env:NELSON_IMAGE_REFERENCE_EXE
)

$ErrorActionPreference = 'Stop'

if ([string]::IsNullOrWhiteSpace($ReferenceExe)) {
  throw 'Set NELSON_IMAGE_REFERENCE_EXE to the reference engine executable.'
}

if (-not (Test-Path -LiteralPath $ReferenceExe)) {
  throw "Reference executable not found: $ReferenceExe"
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = Resolve-Path (Join-Path $scriptDir '..\..\..\..')
$tempDir = Join-Path $repoRoot 'temp'
New-Item -ItemType Directory -Force -Path $tempDir | Out-Null

$generatorFile = Join-Path $tempDir 'generate_image_processing_reference_core.m'
$outputFile = Join-Path $scriptDir 'image_processing_reference_core.mat'
$generatorPath = ($generatorFile -replace '\\', '/')
$outputPath = ($outputFile -replace '\\', '/')

$code = @"
fixtures = struct();
fixtures.available = struct();
outputFile = '$outputPath';

RGB_FIXTURE = zeros(2, 2, 3);
RGB_FIXTURE(:,:, 1) = [1 0; 0 1];
RGB_FIXTURE(:,:, 2) = [0 1; 0 1];
RGB_FIXTURE(:,:, 3) = [0 0; 1 1];
MAP = [1 0 0; 0 1 0; 0 0 1; 0.2 0.3 0.9];

fixtures.available.im2double = exist('im2double', 'file') ~= 0;
if fixtures.available.im2double
  try
    fixtures.im2double_uint8 = im2double(uint8([0 64 128 255]));
    fixtures.im2double_uint16 = im2double(uint16([0 32768 65535]));
    fixtures.im2double_int16 = im2double(int16([-32768 0 32767]));
    fixtures.im2double_complex_double = im2double([1 + 2i 0]);
    fixtures.im2double_sparse_double = im2double(sparse([1 0; 0 2]));
    fixtures.im2double_indexed_uint8 = im2double(uint8([0 1 255]), 'indexed');
    fixtures.im2double_indexed_single = im2double(single([1 2 256]), 'indexed');
  catch
    fixtures.available.im2double = false;
  end
end

fixtures.available.rgb2gray = exist('rgb2gray', 'file') ~= 0;
if fixtures.available.rgb2gray
  try
    fixtures.rgb2gray = rgb2gray(RGB_FIXTURE);
    fixtures.rgb2gray_map = rgb2gray(MAP);
    fixtures.rgb2gray_nan = rgb2gray(reshape([NaN 0 1], [1 1 3]));
    fixtures.rgb2gray_inf = rgb2gray(reshape([Inf 0 0], [1 1 3]));
  catch
    fixtures.available.rgb2gray = false;
  end
end

fixtures.available.im2gray = exist('im2gray', 'file') ~= 0;
if fixtures.available.im2gray
  try
    fixtures.im2gray_rgb = im2gray(RGB_FIXTURE);
    fixtures.im2gray_gray_uint8 = im2gray(uint8([0 255]));
    fixtures.im2gray_complex_2d = im2gray([1 + 2i 0; 0 3 + 4i]);
    fixtures.im2gray_sparse = im2gray(sparse([1 0; 0 2]));
  catch
    fixtures.available.im2gray = false;
  end
end

fixtures.available.rgb2hsv = exist('rgb2hsv', 'file') ~= 0;
if fixtures.available.rgb2hsv
  try
    fixtures.rgb2hsv = rgb2hsv(RGB_FIXTURE);
    fixtures.rgb2hsv_map = rgb2hsv(MAP);
  catch
    fixtures.available.rgb2hsv = false;
  end
end

fixtures.available.hsv2rgb = exist('hsv2rgb', 'file') ~= 0;
if fixtures.available.hsv2rgb && fixtures.available.rgb2hsv
  try
    fixtures.hsv2rgb = hsv2rgb(fixtures.rgb2hsv);
    fixtures.hsv2rgb_map = hsv2rgb(fixtures.rgb2hsv_map);
    fixtures.hsv2rgb_hue_out = hsv2rgb(reshape([2 2 1], [1 1 3]));
  catch
    fixtures.available.hsv2rgb = false;
  end
end

fixtures.available.ind2rgb = exist('ind2rgb', 'file') ~= 0;
if fixtures.available.ind2rgb
  try
    fixtures.ind2rgb_uint8 = ind2rgb(uint8([0 1 2 255]), MAP);
    fixtures.ind2rgb_double = ind2rgb([0 1 2 5], MAP);
    fixtures.ind2rgb_nonfinite = ind2rgb([NaN Inf -Inf 0 1 4], MAP);
  catch
    fixtures.available.ind2rgb = false;
  end
end

fixtures.available.im2uint = exist('im2uint8', 'file') ~= 0 && exist('im2uint16', 'file') ~= 0 ...
  && exist('im2single', 'file') ~= 0;
if fixtures.available.im2uint
  try
    fixtures.im2uint8_unit = im2uint8([0 0.5 1]);
    fixtures.im2uint8_indexed = im2uint8([1 128 256], 'indexed');
    fixtures.im2uint16_unit = im2uint16([0 0.5 1]);
    fixtures.im2single_uint8 = im2single(uint8([0 128 255]));
    fixtures.im2single_indexed = im2single(uint8([0 1 255]), 'indexed');
  catch
    fixtures.available.im2uint = false;
  end
end

fixtures.available.ycbcr = exist('rgb2ycbcr', 'file') ~= 0 && exist('ycbcr2rgb', 'file') ~= 0;
if fixtures.available.ycbcr
  try
    fixtures.rgb2ycbcr_double = rgb2ycbcr(RGB_FIXTURE);
    fixtures.ycbcr2rgb_double = ycbcr2rgb(fixtures.rgb2ycbcr_double);
    fixtures.rgb2ycbcr_uint8 = rgb2ycbcr(uint8(255 * RGB_FIXTURE));
  catch
    fixtures.available.ycbcr = false;
  end
end

FILTER_INPUT = [1 100 2; 3 4 5; 6 7 8];
fixtures.available.padarray = exist('padarray', 'file') ~= 0;
if fixtures.available.padarray
  try
    fixtures.padarray_replicate = padarray([1 2; 3 4], [1 1], 'replicate');
    fixtures.padarray_symmetric = padarray([1 2; 3 4], [1 1], 'symmetric');
  catch
    fixtures.available.padarray = false;
  end
end

fixtures.available.filtering = exist('fspecial', 'file') ~= 0 && exist('imfilter', 'file') ~= 0 ...
  && exist('medfilt2', 'file') ~= 0;
if fixtures.available.filtering
  try
    fixtures.fspecial_gaussian = fspecial('gaussian', [3 3], 0.5);
    fixtures.imfilter_replicate = imfilter([1 2; 3 4], ones(3, 3) / 9, 'replicate');
    fixtures.medfilt2_zero = medfilt2(FILTER_INPUT, [3 3]);
    fixtures.medfilt2_symmetric = medfilt2(FILTER_INPUT, [3 3], 'symmetric');
  catch
    fixtures.available.filtering = false;
  end
end

fixtures.available.imgaussfilt = exist('imgaussfilt', 'file') ~= 0;
fixtures.available.imgaussfilt_rgba = false;
if fixtures.available.imgaussfilt
  try
    GAUSS_RGB = zeros(2, 2, 3);
    GAUSS_RGB(:,:, 1) = [1 2; 3 4];
    GAUSS_RGB(:,:, 2) = [10 20; 30 40];
    GAUSS_RGB(:,:, 3) = [100 200; 300 400];
    fixtures.imgaussfilt_zero = imgaussfilt([1 2; 3 4], 0.5, ...
      'FilterSize', [3 3], 'Padding', 0);
    fixtures.imgaussfilt_symmetric = imgaussfilt([1 2; 3 4], [0.5 1.0], ...
      'FilterSize', [3 5], 'Padding', 'symmetric');
    fixtures.imgaussfilt_rgb = imgaussfilt(GAUSS_RGB, 0.5, ...
      'FilterSize', [3 3], 'Padding', 0);
    fixtures.imgaussfilt_uint8_identity = imgaussfilt(uint8([1 2; 3 4]), 0.5, ...
      'FilterSize', [1 1]);
  catch
    fixtures.available.imgaussfilt = false;
  end
end
if fixtures.available.imgaussfilt
  try
    GAUSS_RGBA = zeros(2, 2, 4);
    GAUSS_RGBA(:,:, 1) = [1 2; 3 4];
    GAUSS_RGBA(:,:, 2) = [10 20; 30 40];
    GAUSS_RGBA(:,:, 3) = [100 200; 300 400];
    GAUSS_RGBA(:,:, 4) = [0.1 0.2; 0.3 0.4];
    fixtures.imgaussfilt_rgba = imgaussfilt(GAUSS_RGBA, 0.5, ...
      'FilterSize', [3 3], 'Padding', 0);
    fixtures.imgaussfilt_rgba_uint8_identity = imgaussfilt(uint8(10 * GAUSS_RGBA), ...
      0.5, 'FilterSize', [1 1]);
    fixtures.available.imgaussfilt_rgba = true;
  catch
    fixtures.available.imgaussfilt_rgba = false;
  end
end

fixtures.available.imresize3 = exist('imresize3', 'file') ~= 0;
if fixtures.available.imresize3
  try
    RESIZE3_INPUT = reshape(1:8, [2 2 2]);
    RESIZE3_SINGLE = single(RESIZE3_INPUT);
    RESIZE3_SINGLE(1, 1, 1) = NaN;
    RESIZE3_SINGLE(2, 2, 2) = Inf;
    fixtures.imresize3_nearest = imresize3(RESIZE3_INPUT, 2, 'nearest');
    fixtures.imresize3_linear = imresize3(RESIZE3_INPUT, [3 3 3], 'linear');
    fixtures.imresize3_uint8_linear = imresize3(uint8(10 * RESIZE3_INPUT), [3 3 3], ...
      'linear');
    fixtures.imresize3_single_nonfinite = imresize3(RESIZE3_SINGLE, [2 2 2], ...
      'nearest');
    RESIZE3_COMPLEX = cat(3, [1 + 1i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
    fixtures.imresize3_complex_nearest = imresize3(RESIZE3_COMPLEX, [2 2 2], ...
      'nearest');
    fixtures.imresize3_complex_linear = imresize3(RESIZE3_COMPLEX, [3 3 3], ...
      'linear');
  catch
    fixtures.available.imresize3 = false;
  end
end

fixtures.available.imgaussfilt3 = exist('imgaussfilt3', 'file') ~= 0;
if fixtures.available.imgaussfilt3
  try
    GAUSS3_INPUT = reshape(1:8, [2 2 2]);
    GAUSS3_IMPULSE = zeros(3, 3, 3);
    GAUSS3_IMPULSE(2, 2, 2) = 1;
    fixtures.imgaussfilt3_identity = imgaussfilt3(GAUSS3_INPUT, 0.5, ...
      'FilterSize', [1 1 1]);
    fixtures.imgaussfilt3_impulse = imgaussfilt3(GAUSS3_IMPULSE, 0.5, ...
      'FilterSize', [3 3 3], 'Padding', 0);
    fixtures.imgaussfilt3_single_replicate = imgaussfilt3(single(GAUSS3_INPUT), ...
      [0.5 0.75 1], 'FilterSize', [3 3 5], 'Padding', 'replicate');
    fixtures.imgaussfilt3_uint8_identity = imgaussfilt3(uint8(GAUSS3_INPUT), 0.5, ...
      'FilterSize', [1 1 1]);
    GAUSS3_COMPLEX = cat(3, [1 + 1i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
    fixtures.imgaussfilt3_complex_identity = imgaussfilt3(GAUSS3_COMPLEX, 0.5, ...
      'FilterSize', [1 1 1]);
  catch
    fixtures.available.imgaussfilt3 = false;
  end
end

MORPH_INPUT = logical([0 1 0; 0 1 0; 0 0 0]);
fixtures.available.morphology = exist('strel', 'file') ~= 0 && exist('imdilate', 'file') ~= 0 ...
  && exist('imerode', 'file') ~= 0 && exist('bwperim', 'file') ~= 0;
if fixtures.available.morphology
  try
    se = strel('square', 3);
    fixtures.imdilate_square = imdilate(MORPH_INPUT, se);
    fixtures.imerode_square = imerode(true(3, 3), se);
    fixtures.bwperim_square = bwperim(true(3, 3));
  catch
    fixtures.available.morphology = false;
  end
end
fixtures.available.bwmorph = exist('bwmorph', 'file') ~= 0;
if fixtures.available.bwmorph
  try
    DILATE_SEED = false(5, 5);
    DILATE_SEED(3, 3) = true;
    OPEN_INPUT = false(5, 5);
    OPEN_INPUT(2:4, 2:4) = true;
    OPEN_INPUT(1, 1) = true;
    CLOSE_INPUT = false(5, 5);
    CLOSE_INPUT(2:4, 2:4) = true;
    CLOSE_INPUT(3, 3) = false;
    fixtures.bwmorph_dilate = bwmorph(DILATE_SEED, 'dilate');
    fixtures.bwmorph_dilate_2 = bwmorph(DILATE_SEED, 'dilate', 2);
    fixtures.bwmorph_erode = bwmorph(true(5, 5), 'erode');
    fixtures.bwmorph_open = bwmorph(OPEN_INPUT, 'open');
    fixtures.bwmorph_close = bwmorph(CLOSE_INPUT, 'close');
    fixtures.bwmorph_tophat = bwmorph(OPEN_INPUT, 'tophat');
    fixtures.bwmorph_bothat = bwmorph(CLOSE_INPUT, 'bothat');
    fixtures.bwmorph_int8_clean = bwmorph(int8([1 0; 0 1]), 'clean');
  catch
    fixtures.available.bwmorph = false;
  end
end

REGION_INPUT = logical([1 0 0 1; 1 0 0 0; 0 0 1 1]);
REGION_INTENSITY = [10 0 0 2; 20 0 0 0; 0 0 30 40];
fixtures.available.regionprops = exist('regionprops', 'file') ~= 0;
if fixtures.available.regionprops
  try
    fixtures.regionprops_basic = regionprops(REGION_INPUT, REGION_INTENSITY, 'Area', ...
      'Centroid', 'BoundingBox', 'MeanIntensity');
    fixtures.regionprops_axis = regionprops(logical([1 1 1]), 'Orientation', ...
      'MajorAxisLength', 'MinorAxisLength', 'Eccentricity');
  catch
    fixtures.available.regionprops = false;
  end
end

fixtures.available.bwconncomp3 = exist('bwconncomp', 'file') ~= 0 ...
  && exist('labelmatrix', 'file') ~= 0;
if fixtures.available.bwconncomp3
  try
    BW3_FIXTURE = false(3, 3, 3);
    BW3_FIXTURE(2, 2, 1) = true;
    BW3_FIXTURE(2, 2, 2) = true;
    BW3_FIXTURE(1, 1, 3) = true;
    CC3_6 = bwconncomp(BW3_FIXTURE, 6);
    CC3_26 = bwconncomp(BW3_FIXTURE, 26);
    fixtures.bwconncomp3_6_numobjects = CC3_6.NumObjects;
    fixtures.bwconncomp3_6_labelmatrix = labelmatrix(CC3_6);
    fixtures.bwconncomp3_26_numobjects = CC3_26.NumObjects;
    fixtures.bwconncomp3_26_labelmatrix = labelmatrix(CC3_26);
  catch
    fixtures.available.bwconncomp3 = false;
  end
end

fixtures.available.regionprops3 = exist('regionprops3', 'file') ~= 0;
if fixtures.available.regionprops3
  try
    REGION3_INPUT = false(5, 6, 4);
    REGION3_INPUT(3:4, 2:4, 3) = true;
    REGION3_INPUT(1, 6, 1) = true;
    REGION3_INTENSITY = reshape(1:numel(REGION3_INPUT), size(REGION3_INPUT));
    REGION3_STATS = regionprops3(REGION3_INPUT, 'Volume', 'Centroid', ...
      'BoundingBox', 'Extent', 'EquivDiameter');
    REGION3_INTENSITY_STATS = regionprops3(REGION3_INPUT, REGION3_INTENSITY, ...
      'MeanIntensity', 'MinIntensity', 'MaxIntensity', 'WeightedCentroid');
    fixtures.regionprops3_volume = local_prop_column(REGION3_STATS, 'Volume');
    fixtures.regionprops3_centroid = local_prop_matrix(REGION3_STATS, 'Centroid');
    fixtures.regionprops3_boundingbox = local_prop_matrix(REGION3_STATS, 'BoundingBox');
    fixtures.regionprops3_extent = local_prop_column(REGION3_STATS, 'Extent');
    fixtures.regionprops3_equivdiameter = local_prop_column(REGION3_STATS, 'EquivDiameter');
    fixtures.regionprops3_meanintensity = local_prop_column(REGION3_INTENSITY_STATS, ...
      'MeanIntensity');
    fixtures.regionprops3_minintensity = local_prop_column(REGION3_INTENSITY_STATS, ...
      'MinIntensity');
    fixtures.regionprops3_maxintensity = local_prop_column(REGION3_INTENSITY_STATS, ...
      'MaxIntensity');
    fixtures.regionprops3_weightedcentroid = local_prop_matrix(REGION3_INTENSITY_STATS, ...
      'WeightedCentroid');
  catch
    fixtures.available.regionprops3 = false;
  end
end

THRESH_INPUT = [0.1 0.1 0.1; 0.1 0.9 0.1; 0.1 0.1 0.1];
fixtures.available.thresholding = exist('graythresh', 'file') ~= 0 && exist('adaptthresh', 'file') ~= 0 ...
  && exist('imbinarize', 'file') ~= 0;
if fixtures.available.thresholding
  try
    [fixtures.graythresh_level, fixtures.graythresh_metric] = graythresh([0 0 1 1]);
    fixtures.adaptthresh_bright = adaptthresh(THRESH_INPUT, 0.4, 'ForegroundPolarity', 'bright');
    fixtures.imbinarize_adaptive = imbinarize(THRESH_INPUT, 'adaptive', 'Sensitivity', 0.4);
  catch
    fixtures.available.thresholding = false;
  end
end

fixtures.available.watershed = exist('watershed', 'file') ~= 0;
fixtures.available.watershed3d = false;
if fixtures.available.watershed
  try
    WS_INPUT = [1 2 3; 2 3 2; 3 2 1];
    WS_PLATEAU = [1 2 1; 2 2 2; 1 2 1];
    WS_C4 = logical([0 1 0; 1 1 1; 0 1 0]);
    fixtures.watershed_4 = watershed(WS_INPUT, 4);
    fixtures.watershed_8 = watershed(WS_INPUT, 8);
    fixtures.watershed_c4 = watershed(WS_INPUT, WS_C4);
    fixtures.watershed_uint8_4 = watershed(uint8(WS_INPUT), 4);
    fixtures.watershed_int8_4 = watershed(int8(WS_INPUT), 4);
    fixtures.watershed_plateau_4 = watershed(WS_PLATEAU, 4);
  catch
    fixtures.available.watershed = false;
  end
end
if fixtures.available.watershed
  try
    WS3_INPUT = ones(2, 2, 2);
    WS3_C6 = false(3, 3, 3);
    WS3_C6(2, 2, 2) = true;
    WS3_C6(1, 2, 2) = true; WS3_C6(3, 2, 2) = true;
    WS3_C6(2, 1, 2) = true; WS3_C6(2, 3, 2) = true;
    WS3_C6(2, 2, 1) = true; WS3_C6(2, 2, 3) = true;
    fixtures.watershed3d_default = watershed(WS3_INPUT);
    fixtures.watershed3d_6 = watershed(WS3_INPUT, 6);
    fixtures.watershed3d_18 = watershed(WS3_INPUT, 18);
    fixtures.watershed3d_26 = watershed(uint8(WS3_INPUT), 26);
    fixtures.watershed3d_c6 = watershed(WS3_INPUT, WS3_C6);
    fixtures.available.watershed3d = true;
  catch
    fixtures.available.watershed3d = false;
  end
end

fixtures.available.activecontour = exist('activecontour', 'file') ~= 0;
fixtures.available.activecontour_rgb = false;
if fixtures.available.activecontour
  try
    AC_INPUT = zeros(7, 7);
    AC_INPUT(3:5, 3:5) = 1;
    AC_MASK = false(7, 7);
    AC_MASK(4, 4) = true;
    AC_NUMERIC_MASK = double(AC_MASK);
    fixtures.activecontour_default = activecontour(AC_INPUT, AC_MASK, 8);
    fixtures.activecontour_int8 = activecontour(int8(AC_INPUT * 10), AC_MASK, 8);
    fixtures.activecontour_edge = activecontour(AC_INPUT, AC_MASK, 8, 'edge');
    fixtures.activecontour_name_value = activecontour(single(AC_INPUT), AC_NUMERIC_MASK, ...
      'Iterations', uint8(8), 'Method', 'chan-vese', 'SmoothFactor', true, ...
      'ContractionBias', int8(0));
  catch
    fixtures.available.activecontour = false;
  end
end
if fixtures.available.activecontour
  try
    AC_INPUT = zeros(7, 7);
    AC_INPUT(3:5, 3:5) = 1;
    AC_MASK = false(7, 7);
    AC_MASK(4, 4) = true;
    AC_RGB = cat(3, AC_INPUT, AC_INPUT, AC_INPUT);
    AC_RGBA = cat(3, AC_RGB, 0.5 * ones(size(AC_INPUT)));
    fixtures.activecontour_rgb = activecontour(AC_RGB, AC_MASK, 8);
    fixtures.activecontour_rgba_edge = activecontour(AC_RGBA, AC_MASK, 8, 'edge');
    fixtures.available.activecontour_rgb = true;
  catch
    fixtures.available.activecontour_rgb = false;
  end
end

fixtures.available.features = exist('cornermetric', 'file') ~= 0 ...
  && exist('detectHarrisFeatures', 'file') ~= 0 ...
  && exist('detectFASTFeatures', 'file') ~= 0;
if fixtures.available.features
  try
    FEATURE_INPUT = zeros(32, 32);
    FEATURE_INPUT(10:22, 10:22) = 1;
    FEATURE_RGB = zeros(32, 32, 3);
    FEATURE_RGB(:,:, 1) = FEATURE_INPUT;
    FEATURE_RGB(:,:, 2) = 0.5 * FEATURE_INPUT;
    fixtures.cornermetric_harris = cornermetric(FEATURE_INPUT, 'Harris', 'FilterSize', 5);
    fixtures.cornermetric_mineigen = cornermetric(FEATURE_RGB, 'Minimum Eigenvalue', ...
      'FilterSize', 5);
    HARRIS_POINTS = detectHarrisFeatures(FEATURE_INPUT, 'MinQuality', 0.05, ...
      'ROI', [8 8 6 6]);
    FAST_POINTS = detectFASTFeatures(FEATURE_RGB, 'MinContrast', 0.2, ...
      'MinQuality', 0, 'ROI', [8 8 18 18]);
    fixtures.detect_harris_roi_count = HARRIS_POINTS.Count;
    fixtures.detect_harris_roi_location = HARRIS_POINTS.Location;
    fixtures.detect_harris_roi_metric = HARRIS_POINTS.Metric;
    fixtures.detect_fast_rgb_count = FAST_POINTS.Count;
    fixtures.detect_fast_rgb_location = FAST_POINTS.Location;
    fixtures.detect_fast_rgb_metric = FAST_POINTS.Metric;
  catch
    fixtures.available.features = false;
  end
end

fixtures.available.registration = exist('imregcorr', 'file') ~= 0 ...
  && exist('imregconfig', 'file') ~= 0 ...
  && exist('imregtform', 'file') ~= 0 ...
  && exist('imregister', 'file') ~= 0;
if fixtures.available.registration
  try
    REG_MOVING = zeros(16, 16);
    REG_MOVING(5:8, 6:9) = 1;
    REG_FIXED = zeros(16, 16);
    REG_FIXED(3:6, 9:12) = 1;
    REG_RGB = cat(3, REG_MOVING, 0.5 * REG_MOVING, 0.25 * REG_MOVING);
    REG_RGB_FIXED = cat(3, REG_FIXED, 0.5 * REG_FIXED, 0.25 * REG_FIXED);
    [REG_CORR_TFORM, fixtures.imregcorr_peak] = imregcorr(REG_MOVING, REG_FIXED, ...
      'translation');
    fixtures.imregcorr_translation_t = local_tform_matrix(REG_CORR_TFORM);
    fixtures.imregcorr_rgb_t = local_tform_matrix(imregcorr(REG_RGB, REG_RGB_FIXED, ...
      'translation'));
    [REG_OPT, REG_MET] = imregconfig('monomodal');
    fixtures.imregconfig_optimizer_type = REG_OPT.Type;
    fixtures.imregconfig_metric_type = REG_MET.Type;
    REG_TFORM = imregtform(REG_MOVING, REG_FIXED, 'translation', REG_OPT, REG_MET, ...
      'Interpolation', 'nearest', 'PyramidLevels', 1, 'DisplayOptimization', false);
    fixtures.imregtform_translation_t = local_tform_matrix(REG_TFORM);
    fixtures.imregister_translation = imregister(REG_MOVING, REG_FIXED, 'translation', ...
      REG_OPT, REG_MET, 'Interpolation', 'nearest', 'PyramidLevels', 1, ...
      'DisplayOptimization', false);
  catch
    fixtures.available.registration = false;
  end
end

fixtures.available.geometry = exist('imwarp', 'file') ~= 0 && exist('affine2d', 'file') ~= 0 ...
  && exist('imref2d', 'file') ~= 0;
fixtures.available.geometry_extended = false;
fixtures.available.geometry_rgba = false;
if fixtures.available.geometry
  try
    GEOM_INPUT = reshape(1:9, 3, 3);
    fixtures.imref2d_default = imref2d([3 4]);
    fixtures.imwarp_identity = imwarp(GEOM_INPUT, affine2d(eye(3)), 'nearest');
    fixtures.imwarp_ref = imwarp(GEOM_INPUT, affine2d(eye(3)), 'nearest', ...
      'OutputView', fixtures.imref2d_default);
    fixtures.imref2d_source = imref2d([3 3], [10.5 13.5], [20.5 23.5]);
    [fixtures.imwarp_source_ref, fixtures.imwarp_source_world] = imwarp(GEOM_INPUT, ...
      fixtures.imref2d_source, affine2d(eye(3)), 'nearest');
  catch
    fixtures.available.geometry = false;
  end
end
if fixtures.available.geometry
  try
    GEOM_INPUT = reshape(1:9, 3, 3);
    GEOM_RGB = zeros(2, 2, 3);
    GEOM_RGB(:,:, 1) = [1 2; 3 4];
    GEOM_RGB(:,:, 2) = [10 20; 30 40];
    GEOM_RGB(:,:, 3) = [100 200; 300 400];
    fixtures.imwarp_numeric_view_uint8 = imwarp(uint8(GEOM_INPUT), affine2d(eye(3)), ...
      'nearest', 'OutputView', [4 5]);
    fixtures.imwarp_linear_fill = imwarp(GEOM_INPUT, [1 0 0; 0 1 0; 0.5 0 1], ...
      'linear', 'FillValues', -1);
    fixtures.imwarp_rgb_fill = imwarp(GEOM_RGB, [1 0 0; 0 1 0; 1 0 1], ...
      'nearest', 'FillValues', [4 5 6]);
    GEOM_SRC_REF = imref2d([3 3], [10.5 13.5], [20.5 23.5]);
    [fixtures.imwarp_source_full, fixtures.imwarp_source_full_ref] = imwarp(GEOM_INPUT, ...
      GEOM_SRC_REF, affine2d([1 0 0; 0 1 0; 1 0 1]), 'nearest', 'OutputView', 'full');
    fixtures.available.geometry_extended = true;
  catch
    fixtures.available.geometry_extended = false;
  end
end
if fixtures.available.geometry
  try
    GEOM_RGBA = zeros(2, 2, 4);
    GEOM_RGBA(:,:, 1) = [1 2; 3 4];
    GEOM_RGBA(:,:, 2) = [10 20; 30 40];
    GEOM_RGBA(:,:, 3) = [100 200; 300 400];
    GEOM_RGBA(:,:, 4) = [0.1 0.2; 0.3 0.4];
    fixtures.imwarp_rgba_fill = imwarp(GEOM_RGBA, [1 0 0; 0 1 0; 1 0 1], ...
      'nearest', 'FillValues', [4 5 6 7]);
    fixtures.imwarp_rgba_uint8_identity = imwarp(uint8(10 * GEOM_RGBA), affine2d(eye(3)), ...
      'nearest');
    fixtures.available.geometry_rgba = true;
  catch
    fixtures.available.geometry_rgba = false;
  end
end

fixtures.available.geometry3d = exist('imwarp', 'file') ~= 0 ...
  && exist('affine3d', 'file') ~= 0 ...
  && exist('imref3d', 'file') ~= 0;
if fixtures.available.geometry3d
  try
    GEOM3_INPUT = reshape(1:27, [3 3 3]);
    GEOM3_REF = imref3d([3 4 5]);
    fixtures.imref3d_imagesize = GEOM3_REF.ImageSize;
    fixtures.imref3d_xworldlimits = GEOM3_REF.XWorldLimits;
    fixtures.imref3d_yworldlimits = GEOM3_REF.YWorldLimits;
    fixtures.imref3d_zworldlimits = GEOM3_REF.ZWorldLimits;
    fixtures.affine3d_identity_t = local_tform_matrix(affine3d(eye(4)));
    fixtures.imwarp3d_identity = imwarp(GEOM3_INPUT, affine3d(eye(4)), 'nearest');
    fixtures.imwarp3d_uint8_identity = imwarp(uint8(GEOM3_INPUT), affine3d(eye(4)), ...
      'nearest');
    GEOM3_COMPLEX = cat(3, [1 + 1i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
    fixtures.imwarp3d_complex_identity = imwarp(GEOM3_COMPLEX, affine3d(eye(4)), ...
      'nearest');
    GEOM3_SRC_REF = imref3d([3 3 3], [10.5 13.5], [20.5 23.5], [30.5 33.5]);
    [fixtures.imwarp3d_source_ref, GEOM3_SOURCE_WORLD] = imwarp(GEOM3_INPUT, ...
      GEOM3_SRC_REF, affine3d(eye(4)), 'nearest');
    fixtures.imwarp3d_source_world_imagesize = GEOM3_SOURCE_WORLD.ImageSize;
    fixtures.imwarp3d_source_world_xlimits = GEOM3_SOURCE_WORLD.XWorldLimits;
    fixtures.imwarp3d_source_world_ylimits = GEOM3_SOURCE_WORLD.YWorldLimits;
    fixtures.imwarp3d_source_world_zlimits = GEOM3_SOURCE_WORLD.ZWorldLimits;
    fixtures.imwarp3d_numeric_view = imwarp(GEOM3_INPUT, affine3d(eye(4)), ...
      'nearest', 'OutputView', [4 4 4]);
  catch
    fixtures.available.geometry3d = false;
  end
end

fixtures.available.imrotate = exist('imrotate', 'file') ~= 0;
fixtures.available.imrotate_rgba = false;
if fixtures.available.imrotate
  try
    ROT_INPUT = reshape(1:12, 3, 4);
    ROT_RGB = zeros(4, 4, 3);
    ROT_RGB(:,:, 1) = reshape(1:16, 4, 4);
    ROT_RGB(:,:, 2) = 10 + reshape(1:16, 4, 4);
    ROT_RGB(:,:, 3) = 20 + reshape(1:16, 4, 4);
    fixtures.imrotate_uint16_90_loose = imrotate(uint16(ROT_INPUT), 90, 'nearest', 'loose');
    fixtures.imrotate_int8_90_loose = imrotate(int8(ROT_INPUT) - 7, 90, ...
      'nearest', 'loose');
    fixtures.imrotate_rgb_bilinear_crop = imrotate(ROT_RGB, 33, 'bilinear', 'crop');
    fixtures.imrotate_single_nonfinite = imrotate(single([1 NaN; Inf 4]), 180, 'nearest', 'loose');
  catch
    fixtures.available.imrotate = false;
  end
end
if fixtures.available.imrotate
  try
    ROT_RGBA = zeros(4, 4, 4);
    ROT_RGBA(:,:, 1:3) = ROT_RGB;
    ROT_RGBA(:,:, 4) = reshape((1:16) / 16, 4, 4);
    fixtures.imrotate_rgba_nearest_crop = imrotate(ROT_RGBA, -45, 'nearest', 'crop');
    fixtures.available.imrotate_rgba = true;
  catch
    fixtures.available.imrotate_rgba = false;
  end
end

fixtures.available.error_cases = struct();
fixtures.error_cases = struct();

fixtures.available.error_cases.imgaussfilt = exist('imgaussfilt', 'file') ~= 0;
if fixtures.available.error_cases.imgaussfilt
  fixtures.error_cases.imgaussfilt_bad_padding = local_throws(@() ...
    imgaussfilt([1 2], 1, 'Padding', Inf));
end

fixtures.available.error_cases.bwmorph = exist('bwmorph', 'file') ~= 0;
if fixtures.available.error_cases.bwmorph
  fixtures.error_cases.bwmorph_bad_operation = local_throws(@() ...
    bwmorph(logical([1 0]), 'unknown'));
end

fixtures.available.error_cases.watershed = exist('watershed', 'file') ~= 0;
if fixtures.available.error_cases.watershed
  fixtures.error_cases.watershed_bad_connectivity = local_throws(@() ...
    watershed([1 2], 6));
end

fixtures.available.error_cases.activecontour = exist('activecontour', 'file') ~= 0;
if fixtures.available.error_cases.activecontour
  fixtures.error_cases.activecontour_bad_mask = local_throws(@() ...
    activecontour([1 2], logical([1; 0])));
end

fixtures.available.error_cases.imwarp = exist('imwarp', 'file') ~= 0;
if fixtures.available.error_cases.imwarp
  fixtures.error_cases.imwarp_bad_fillvalues = local_throws(@() ...
    imwarp(zeros(2, 2, 3), eye(3), 'FillValues', [1 2]));
end

fixtures.available.error_cases.imrotate = exist('imrotate', 'file') ~= 0;
if fixtures.available.error_cases.imrotate
  fixtures.error_cases.imrotate_bad_method = local_throws(@() ...
    imrotate([1 2], 45, 'unknown'));
end

fixtures.available.error_cases.imresize3 = exist('imresize3', 'file') ~= 0;
if fixtures.available.error_cases.imresize3
  fixtures.error_cases.imresize3_bad_size = local_throws(@() ...
    imresize3(ones(2, 2, 2), [2 2]));
end

fixtures.available.error_cases.regionprops3 = exist('regionprops3', 'file') ~= 0;
if fixtures.available.error_cases.regionprops3
  fixtures.error_cases.regionprops3_bad_property = local_throws(@() ...
    regionprops3(true(2, 2, 2), 'UnsupportedProperty'));
end

fixtures.available.error_cases.registration = exist('imregcorr', 'file') ~= 0;
if fixtures.available.error_cases.registration
  fixtures.error_cases.imregcorr_bad_transform = local_throws(@() ...
    imregcorr([1 2], [1 2], 'projective'));
end

save(outputFile, 'fixtures');

function T = local_tform_matrix(tform)
  if isstruct(tform) && isfield(tform, 'T')
    T = tform.T;
    return
  end
  if isobject(tform) && isprop(tform, 'T')
    T = tform.T;
    return
  end
  if isobject(tform) && isprop(tform, 'A')
    T = tform.A;
    return
  end
  error('Unsupported transform object.');
end

function values = local_prop_column(stats, name)
  if istable(stats)
    values = stats.(name);
    if iscell(values)
      values = cell2mat(values);
    end
    return
  end
  values = reshape([stats.(name)], [], 1);
end

function values = local_prop_matrix(stats, name)
  if istable(stats)
    values = stats.(name);
    if iscell(values)
      values = cell2mat(values);
    end
    return
  end
  values = zeros(numel(stats), numel(stats(1).(name)));
  for k = 1:numel(stats)
    values(k,:) = stats(k).(name);
  end
end

function tf = local_throws(func)
  tf = false;
  try
    func();
  catch
    tf = true;
  end
end
"@

Set-Content -LiteralPath $generatorFile -Value $code -NoNewline
& $ReferenceExe -batch "run('$generatorPath')"
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}

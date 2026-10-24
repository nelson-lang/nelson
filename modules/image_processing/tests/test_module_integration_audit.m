%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
moduleRoot = modulepath('image_processing');
skip_testsuite(~isfile([moduleRoot, '/builtin/cpp/Gateway.cpp']), ...
  'Source-tree audit requires unpackaged builtin/src/reference generator files.');
functionFiles = dir([moduleRoot, '/functions/*.m']);
publicNames = {};
for k = 1:numel(functionFiles)
  [~, name, ~] = fileparts(functionFiles(k).name);
  publicNames{end + 1} = name;
end
publicNames = sort(publicNames);
%=============================================================================
testFiles = dir([moduleRoot, '/tests/test_*.m']);
allTestText = '';
for k = 1:numel(testFiles)
  allTestText = [allTestText, char(10), fileread([moduleRoot, '/tests/', testFiles(k).name])];
end
signatureTestText = fileread([moduleRoot, '/tests/test_signatures.m']);
for k = 1:numel(publicNames)
  asserts.istrue(~isempty(strfind(allTestText, publicNames{k})));
  asserts.istrue(~isempty(strfind(signatureTestText, ['''', publicNames{k}, ''''])));
end
%=============================================================================
privateLeaks = dir([moduleRoot, '/functions/ip_*.m']);
asserts.isequal(numel(privateLeaks), 0);
privateHelpers = dir([moduleRoot, '/functions/private/ip_*.m']);
asserts.istrue(numel(privateHelpers) > 0);
%=============================================================================
subdirs = {'', '/1_image_basics', '/1_image_basics/1_image_types_color', ...
  '/1_image_basics/2_contrast_thresholding', '/1_image_basics/3_filtering_edges', ...
  '/2_image_analysis', '/2_image_analysis/4_morphology', ...
  '/2_image_analysis/5_regions_boundaries', '/2_image_analysis/7_segmentation', ...
  '/2_image_analysis/9_feature_detection', '/3_geometry_registration_3d', ...
  '/3_geometry_registration_3d/6_geometric_transforms', ...
  '/3_geometry_registration_3d/8_volumes_3d', ...
  '/3_geometry_registration_3d/9a_image_registration'};
languages = {'en_US', 'fr_FR'};
helpOnlyNames = {'imresize', 'imrotate', 'image_registration'};
expectedHelpNames = sort([publicNames, helpOnlyNames]);
oldHelpNames = {'im2double', 'imresize', 'imrotate'};
placeholderPattern = ['<param_name>input</param_name>|<param_name>output</param_name>|', ...
  'Input arguments for|Output returned by|Arguments d''entree pour|Sortie renvoyee par'];
for langIndex = 1:numel(languages)
  helpNames = {};
  for dirIndex = 1:numel(subdirs)
    xmlDir = [moduleRoot, '/help/', languages{langIndex}, '/xml', subdirs{dirIndex}];
    xmlFiles = dir([xmlDir, '/*.xml']);
    for fileIndex = 1:numel(xmlFiles)
      [~, name, ~] = fileparts(xmlFiles(fileIndex).name);
      if strcmp(name, 'chapter')
        continue;
      end
      helpNames{end + 1} = name;
      filename = [xmlDir, '/', xmlFiles(fileIndex).name];
      txt = fileread(filename);
      asserts.isequal(regexp(txt, placeholderPattern, 'once'), []);
      imageRefs = regexp(txt, 'src="([^"]+\.(png|jpg|svg))"', 'tokens');
      for imageIndex = 1:numel(imageRefs)
        asserts.istrue(isfile([xmlDir, '/', imageRefs{imageIndex}{1}]));
      end
      if ~any(strcmp(name, oldHelpNames))
        asserts.istrue(~isempty(strfind(txt, '<history_version>2.0.0</history_version>')));
      end
    end
  end
  helpNames = sort(helpNames);
  asserts.isequal(numel(setdiff(expectedHelpNames, helpNames)), 0);
  asserts.isequal(numel(setdiff(helpNames, expectedHelpNames)), 0);
end
%=============================================================================
benchSpecs = {...
  {'bench_imrotate.m', 'imrotate_rgb2048_nearest_loose'}, ...
  {'bench_imrotate_bilinear_uint8_rgb.m', 'imrotate_uint8_rgb64_bilinear_crop_360'}, ...
  {'bench_imrotate_display_pipeline.m', 'imrotate_display_rotate_set_cdata_refresh'}, ...
  {'bench_imwarp.m', 'imwarp_double512_linear_same'}, ...
  {'bench_imgaussfilt.m', 'imgaussfilt_double512_sigma15'}, ...
  {'bench_medfilt2.m', 'medfilt2_mixed_256_three_passes'}, ...
  {'bench_bwmorph.m', 'bwmorph_local_ops_256_12_passes'}, ...
  {'bench_bwconncomp_many_small_components.m', 'bwconncomp_many_small_components_512'}, ...
  {'bench_regionprops_axes.m', 'regionprops_axes_512_30_passes'}, ...
  {'bench_regionprops_convexity.m', 'regionprops_convexity_512_10_passes'}, ...
  {'bench_regionprops_pixel_copy.m', 'regionprops_pixel_copy_512_20_passes'}, ...
  {'bench_imresize3.m', 'imresize3_single48x48x24_to_64x64x32'}, ...
  {'bench_imgaussfilt3.m', 'imgaussfilt3_single64x64x32_replicate'}, ...
  {'bench_imwarp3d.m', 'imwarp3d_single48x48x24_linear'}, ...
  {'bench_morphology3d.m', 'morphology3d_sphere64x64x32'}, ...
  {'bench_bwconncomp3d.m', 'bwconncomp3d_mixed_components_64x64x32'}, ...
  {'bench_regionprops3.m', 'regionprops3_two_blobs_64x64x32'} ...
};
for k = 1:numel(benchSpecs)
  benchFile = [moduleRoot, '/tests/', benchSpecs{k}{1}];
  asserts.istrue(isfile(benchFile));
  benchText = fileread(benchFile);
  asserts.istrue(~isempty(strfind(benchText, 'ip_bench_report')));
  asserts.istrue(~isempty(strfind(benchText, benchSpecs{k}{2})));
end
benchHelper = [moduleRoot, '/tests/private/ip_bench_report.m'];
asserts.istrue(isfile(benchHelper));
benchHelperText = fileread(benchHelper);
asserts.istrue(~isempty(strfind(benchHelperText, 'NELSON_IMAGE_PROCESSING_BENCH_STRICT')));
%=============================================================================
imgaussfilt3Text = fileread([moduleRoot, '/functions/imgaussfilt3.m']);
imresize3Text = fileread([moduleRoot, '/functions/imresize3.m']);
imwarp3dText = fileread([moduleRoot, '/functions/private/ip_imwarp3d.m']);
gatewayText = fileread([moduleRoot, '/builtin/cpp/Gateway.cpp']);
asserts.istrue(~isempty(strfind(imgaussfilt3Text, '__imgaussfilt3_kernel__')));
asserts.istrue(~isempty(strfind(imresize3Text, '__imresize3_kernel__')));
asserts.istrue(~isempty(strfind(imwarp3dText, '__imwarp3d_kernel__')));
asserts.istrue(~isempty(strfind(gatewayText, '__imgaussfilt3_kernel__')));
asserts.istrue(~isempty(strfind(gatewayText, '__imresize3_kernel__')));
asserts.istrue(~isempty(strfind(gatewayText, '__imwarp3d_kernel__')));
asserts.istrue(isfile([moduleRoot, '/src/cpp/ImageGaussianFilter3D.cpp']));
asserts.istrue(isfile([moduleRoot, '/src/cpp/ImageResize3D.cpp']));
asserts.istrue(isfile([moduleRoot, '/src/cpp/ImageWarp3D.cpp']));
%=============================================================================
displayBenchFile = [moduleRoot, '/tests/bench_imrotate_display_pipeline.m'];
displayBenchText = fileread(displayBenchFile);
asserts.istrue(~isempty(strfind(displayBenchText, '<--ADV-CLI MODE-->')));
asserts.istrue(~isempty(strfind(displayBenchText, '<--SEQUENTIAL TEST REQUIRED-->')));
displayBenchNames = {...
  'imrotate_display_rotate_only', ...
  'imrotate_display_rgba_rotate_only', ...
  'imrotate_display_set_cdata_only', ...
  'imrotate_display_set_cdata_refresh', ...
  'imrotate_display_rotate_set_cdata', ...
  'imrotate_display_rotate_set_cdata_refresh', ...
  'imrotate_display_rgba_rotate_set_cdata_refresh' ...
};
for k = 1:numel(displayBenchNames)
  asserts.istrue(~isempty(strfind(displayBenchText, displayBenchNames{k})));
end
asserts.istrue(~isempty(strfind(displayBenchText, 'set(ii, ''CData''')));
asserts.istrue(~isempty(strfind(displayBenchText, 'refresh(f)')));
imageRotationText = fileread([moduleRoot, '/src/cpp/ImageRotation.cpp']);
asserts.istrue(~isempty(strfind(imageRotationText, 'bilinearRotateUint8RGB')));
asserts.istrue(~isempty(strfind(imageRotationText, 'bilinearRotateUint8RGBA')));
%=============================================================================
referenceGeneratorFile = [moduleRoot, '/tests/fixtures/generate_image_processing_reference_core.ps1'];
referenceGeneratorText = fileread(referenceGeneratorFile);
expectedReferenceProbes = {...
  'imgaussfilt_uint8_identity', ...
  'imgaussfilt3_uint8_identity', ...
  'imwarp_identity', ...
  'imwarp3d_uint8_identity', ...
  'bwmorph_int8_clean', ...
  'watershed_int8_4', ...
  'activecontour_int8', ...
  'activecontour_default', ...
  'imrotate_int8_90_loose', ...
  'imref2d_default', ...
  'imwarp_source_world', ...
  'imref3d_imagesize', ...
  'imwarp3d_source_world_zlimits' ...
};
for k = 1:numel(expectedReferenceProbes)
  asserts.istrue(~isempty(strfind(referenceGeneratorText, expectedReferenceProbes{k})));
end
%=============================================================================
referenceFile = [moduleRoot, '/tests/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
expectedAvailability = {'im2double', 'rgb2gray', 'im2gray', 'rgb2hsv', 'hsv2rgb', ...
  'ind2rgb', 'im2uint', 'ycbcr', 'padarray', 'filtering', 'imgaussfilt', ...
  'imgaussfilt_rgba', 'imresize3', 'imgaussfilt3', 'morphology', 'bwmorph', ...
  'regionprops', 'bwconncomp3', 'regionprops3', 'thresholding', 'watershed', ...
  'watershed3d', 'activecontour', 'activecontour_rgb', 'features', 'registration', ...
  'geometry', 'geometry_extended', 'geometry_rgba', 'geometry3d', 'imrotate', ...
  'imrotate_rgba', 'error_cases'};
for k = 1:numel(expectedAvailability)
  asserts.istrue(isfield(fixtures.available, expectedAvailability{k}));
end
expectedErrorAvailability = {'imgaussfilt', 'bwmorph', 'watershed', ...
  'activecontour', 'imwarp', 'imrotate', 'imresize3', 'regionprops3', 'registration'};
asserts.istrue(isfield(fixtures.available, 'error_cases'));
for k = 1:numel(expectedErrorAvailability)
  asserts.istrue(isfield(fixtures.available.error_cases, expectedErrorAvailability{k}));
end
errorCaseText = fileread(referenceGeneratorFile);
asserts.istrue(~isempty(strfind(errorCaseText, 'activecontour_rgba_edge')));
%=============================================================================

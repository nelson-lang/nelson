%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'error_cases')
  if isfield(fixtures.available.error_cases, 'imgaussfilt') && fixtures.available.error_cases.imgaussfilt
    asserts.isequal(ip_throws_expr('imgaussfilt([1 2], 1, ''Padding'', Inf)'), ...
      fixtures.error_cases.imgaussfilt_bad_padding);
  end
  if isfield(fixtures.available.error_cases, 'bwmorph') && fixtures.available.error_cases.bwmorph
    asserts.isequal(ip_throws_expr('bwmorph(logical([1 0]), ''unknown'')'), ...
      fixtures.error_cases.bwmorph_bad_operation);
  end
  if isfield(fixtures.available.error_cases, 'watershed') && fixtures.available.error_cases.watershed
    asserts.isequal(ip_throws_expr('watershed([1 2], 6)'), ...
      fixtures.error_cases.watershed_bad_connectivity);
  end
  if isfield(fixtures.available.error_cases, 'activecontour') && fixtures.available.error_cases.activecontour
    asserts.isequal(ip_throws_expr('activecontour([1 2], logical([1; 0]))'), ...
      fixtures.error_cases.activecontour_bad_mask);
  end
  if isfield(fixtures.available.error_cases, 'imwarp') && fixtures.available.error_cases.imwarp
    asserts.isequal(ip_throws_expr('imwarp(zeros(2, 2, 3), eye(3), ''FillValues'', [1 2])'), ...
      fixtures.error_cases.imwarp_bad_fillvalues);
  end
  if isfield(fixtures.available.error_cases, 'imrotate') && fixtures.available.error_cases.imrotate
    asserts.isequal(ip_throws_expr('imrotate([1 2], 45, ''unknown'')'), ...
      fixtures.error_cases.imrotate_bad_method);
    if isfield(fixtures.error_cases, 'imrotate_complex_image')
      asserts.isequal(ip_throws_expr('imrotate([1 + i 2], 45)'), ...
        fixtures.error_cases.imrotate_complex_image);
    end
  end
  if isfield(fixtures.available.error_cases, 'imresize3') && fixtures.available.error_cases.imresize3
    asserts.isequal(ip_throws_expr('imresize3(ones(2, 2, 2), [2 2])'), ...
      fixtures.error_cases.imresize3_bad_size);
  end
  if isfield(fixtures.available.error_cases, 'regionprops3') && fixtures.available.error_cases.regionprops3
    asserts.isequal(ip_throws_expr('regionprops3(true(2, 2, 2), ''UnsupportedProperty'')'), ...
      fixtures.error_cases.regionprops3_bad_property);
  end
  if isfield(fixtures.available.error_cases, 'registration') && fixtures.available.error_cases.registration
    asserts.isequal(ip_throws_expr('imregcorr([1 2], [1 2], ''projective'')'), ...
      fixtures.error_cases.imregcorr_bad_transform);
  end
end
%=============================================================================

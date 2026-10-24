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
V = reshape(1:8, [2 2 2]);
I = imresize3(V, [2 2 2]);
asserts.isequal(I, V);
%=============================================================================
N = imresize3(V, 2, 'nearest');
asserts.isequal(size(N), [4 4 4]);
asserts.isequal(N(1:2, 1:2, 1:2), ones(2, 2, 2));
asserts.isequal(N(3:4, 3:4, 3:4), 8 * ones(2, 2, 2));
%=============================================================================
L = imresize3(V, [3 3 3], 'linear');
asserts.isequal(size(L), [3 3 3]);
asserts.isapprox(L(2, 2, 2), 4.5, 1e-14);
%=============================================================================
L2 = imresize3(V, [3 3 3], 'Method', 'linear', 'Antialiasing', 'off');
asserts.isequal(L2, L);
L3 = imresize3(V, [3 3 3], 'Method', 'linear', 'Antialiasing', single(true));
asserts.isequal(L3, L);
N2 = imresize3(V, [4 4 4], 'Method', 'nearest');
asserts.isequal(N2, N);
%=============================================================================
U = uint8(V * 10);
R = imresize3(U, [3 3 3], 'linear');
asserts.isequal(class(R), 'uint8');
asserts.isequal(R(2, 2, 2), uint8(45));
U32 = uint32(V * 100);
N32 = imresize3(U32, [2 2 2], 'nearest');
asserts.isequal(class(N32), 'uint32');
asserts.isequal(N32, U32);
%=============================================================================
S = single(V);
S(1, 1, 1) = NaN;
S(2, 2, 2) = Inf;
SN = imresize3(S, [2 2 2], 'nearest');
asserts.isequal(class(SN), 'single');
asserts.istrue(isnan(SN(1, 1, 1)));
asserts.istrue(isinf(SN(2, 2, 2)));
%=============================================================================
BW = false(2, 2, 2);
BW(2, 2, 2) = true;
BW2 = imresize3(BW, 2, 'nearest');
asserts.isequal(class(BW2), 'logical');
asserts.isequal(sum(BW2(:)), 8);
%=============================================================================
C = cat(3, [1 + i 2; 3 4 - 2i], [5 6; 7 8 + 3i]);
CN = imresize3(C, [2 2 2], 'nearest');
asserts.isequal(CN, C);
CL = imresize3(C, [3 3 3], 'linear');
asserts.isapprox(real(CL), imresize3(real(C), [3 3 3], 'linear'), 1e-14);
asserts.isapprox(imag(CL), imresize3(imag(C), [3 3 3], 'linear'), 1e-14);
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'imresize3') && fixtures.available.imresize3
  asserts.isequal(imresize3(V, 2, 'nearest'), fixtures.imresize3_nearest);
  asserts.isapprox(imresize3(V, [3 3 3], 'linear'), fixtures.imresize3_linear, 1e-12);
  asserts.isequal(imresize3(uint8(10 * V), [3 3 3], 'linear'), ...
    fixtures.imresize3_uint8_linear);
  asserts.isequal(imresize3(S, [2 2 2], 'nearest'), fixtures.imresize3_single_nonfinite);
  if isfield(fixtures, 'imresize3_complex_nearest')
    asserts.isequal(imresize3(C, [2 2 2], 'nearest'), fixtures.imresize3_complex_nearest);
    asserts.isapprox(imresize3(C, [3 3 3], 'linear'), fixtures.imresize3_complex_linear, 1e-12);
  end
end
%=============================================================================
asserts.checkerror('imresize3([1 2], 2)', sprintf(_('%s must be a 3-D array.'), 'Volume'));
%=============================================================================
asserts.isequal(imresize3(cat(3, [1 2], [3 + i 4]), [1 2 2], 'nearest'), ...
  cat(3, [1 2], [3 + i 4]));
%=============================================================================
asserts.checkerror('imresize3(sparse([1 2]), 2)', sprintf(_('%s are not supported.'), 'Sparse volumes'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), 0)', _('Scale factor must be positive and finite.'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), [2 2])', _('Scale factor must be a scalar or output size a 3-element vector.'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), [2 Inf 2])', sprintf(_('%s must contain positive finite values.'), 'Output size'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), 2, ''cubic'')', sprintf(_('Unsupported "%s" option.'), 'interpolation method'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), 2, ''Method'', 1)', sprintf(_('%s must be a string.'), 'Interpolation method'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), 2, ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'imresize3'));
%=============================================================================
asserts.checkerror('imresize3(ones(2, 2, 2), 2, 1, true)', sprintf(_('%s option name must be a string.'), 'imresize3'));

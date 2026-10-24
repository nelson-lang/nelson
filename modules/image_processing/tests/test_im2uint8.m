%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = uint8([0 128 255]);
asserts.isequal(im2uint8([0 0.5 1]), uint8([0 128 255]));
asserts.isequal(im2uint16([0 1]), uint16([0 65535]));
asserts.isequal(im2single(uint8([0 255])), single([0 1]));
asserts.isequal(im2uint8(single([1 2 256]), 'indexed'), uint8([0 1 255]));
asserts.isequal(im2uint16(single([1 2 65536]), 'indexed'), uint16([0 1 65535]));
asserts.isequal(im2single(single([1 2 256]), 'indexed'), single([1 2 256]));
asserts.checkerror('im2uint8(int8([-128 127]))', _('Image must be double, logical, uint8, uint16, int16, or single.'));
asserts.checkerror('im2uint16(uint32([0 intmax(''uint32'')]))', _('Image must be double, logical, uint8, uint16, int16, or single.'));
asserts.checkerror('im2uint8(int16([0 1]), ''indexed'')', ...
  _('An indexed image must have a data type of ''uint8'', ''uint16'', ''double'', ''single'', or ''logical''.'));
asserts.isequal(imcomplement(A), uint8([255 127 0]));
asserts.isequal(imcomplement(int8([-128 0 127])), int8([127 -1 -128]));
asserts.isequal(imcomplement(int16([-32768 0 32767])), int16([32767 -1 -32768]));
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if isfield(fixtures.available, 'im2uint') && fixtures.available.im2uint
  asserts.isequal(im2uint8([0 0.5 1]), fixtures.im2uint8_unit);
  asserts.isequal(im2uint8([1 128 256], 'indexed'), fixtures.im2uint8_indexed);
  asserts.isequal(im2uint16([0 0.5 1]), fixtures.im2uint16_unit);
  asserts.isapprox(im2single(uint8([0 128 255])), fixtures.im2single_uint8, 1e-7);
  asserts.isequal(im2single(uint8([0 1 255]), 'indexed'), fixtures.im2single_indexed);
end
if isfield(fixtures.available, 'ycbcr') && fixtures.available.ycbcr
  asserts.isapprox(rgb2ycbcr(RGB), fixtures.rgb2ycbcr_double, 1e-12);
  asserts.isapprox(ycbcr2rgb(fixtures.rgb2ycbcr_double), fixtures.ycbcr2rgb_double, 1e-5);
  asserts.isequal(rgb2ycbcr(uint8(255 * RGB)), fixtures.rgb2ycbcr_uint8);
end
%=============================================================================
I = reshape(uint16(linspace(0, 65535, 25)), [5 5]);
J = im2uint8(I);
REF = uint8([0 53 106 159 213; 11 64 117 170 223; 21 74 128 181 234; ...
  32 85 138 191 244; 43 96 149 202 255]);
asserts.isequal(J, REF);
%=============================================================================
asserts.isequal(im2uint8(uint8([0 255]), 'indexed'), uint8([0 255]));
asserts.isequal(im2uint8(uint16([0 255]), 'indexed'), uint8([0 255]));
asserts.isequal(im2uint8([1 256], 'indexed'), uint8([0 255]));
asserts.isequal(im2uint16(uint8([0 255]), 'indexed'), uint16([0 255]));
asserts.isequal(im2uint16([1 65536], 'indexed'), uint16([0 65535]));
asserts.isequal(im2single(uint8([0 255]), 'indexed'), single([1 256]));
asserts.isequal(im2single(logical([0 1]), 'indexed'), single([0 1]));
asserts.checkerror('im2uint8(uint16(256), ''indexed'')', _('Indexed uint16 image values must be less than or equal to 255.'));
asserts.checkerror('im2uint8([0 1], ''indexed'')', sprintf(_('Indexed double image values must be in the range [%d, %d].'), 1, 256));
asserts.checkerror('im2uint16([0 1], ''indexed'')', sprintf(_('Indexed double image values must be in the range [%d, %d].'), 1, 65536));
asserts.isequal(im2single(single([1 2]), 'indexed'), single([1 2]));
asserts.isequal(im2gray([1 2; 3 4]), [1 2; 3 4]);

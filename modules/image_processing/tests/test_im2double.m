%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
R = im2double(single([3 25]), 'indexed');
REF = [3 25];
asserts.isequal(R, REF);
%=============================================================================
R = im2double([3 25], 'indexed');
REF = [3 25];
asserts.isequal(R, REF);
%=============================================================================
R = im2double(uint16([0 1 257]), 'indexed');
REF = [1 2 258];
asserts.isequal(R, REF);
%=============================================================================
R = im2double(uint8([0 1 255]), 'indexed');
REF = [1 2 256];
asserts.isequal(R, REF);
%=============================================================================
R = im2double([10 20 30]);
REF = [10 20 30];
asserts.isequal(R, REF);
%=============================================================================
R = im2double(single([10 20 30]));
REF = [10 20 30];
asserts.isequal(R, REF);
%=============================================================================
R = im2double(uint8([0 127 128 255]));
REF = [0 0.4980 0.5020 1.0000];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = im2double(uint16([0 127 128 65535]));
REF = [0 0.0019 0.0020 1.0000];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = im2double(int16([-32768 -32767 -32766 32767]));
REF = [0 0.0000 0.0000 1.0000];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
asserts.checkerror('im2double(int8([-128 127]))', _('Image must be double, logical, uint8, uint16, int16, or single.'));
asserts.checkerror('im2double(uint32([0 intmax(''uint32'')]))', _('Image must be double, logical, uint8, uint16, int16, or single.'));
asserts.checkerror('im2double(int16([0 1]), ''indexed'')', ...
  _('An indexed image must have a data type of ''uint8'', ''uint16'', ''double'', ''single'', or ''logical''.'));
asserts.checkerror('im2double(int8([0 1]), ''indexed'')', _('Image must be double, logical, uint8, uint16, int16, or single.'));
asserts.checkerror('im2double(''ab'', ''indexed'')', _('Image must be double, logical, uint8, uint16, int16, or single.'));
%=============================================================================
referenceFile = [fileparts(nfilename('fullpathext'), 'path'), '/fixtures/image_processing_reference_core.mat'];
asserts.istrue(isfile(referenceFile));
load(referenceFile);
if fixtures.available.im2double
  asserts.isapprox(im2double(uint8([0 64 128 255])), fixtures.im2double_uint8, 1e-15);
  asserts.isapprox(im2double(uint16([0 32768 65535])), fixtures.im2double_uint16, 1e-15);
  asserts.isapprox(im2double(int16([-32768 0 32767])), fixtures.im2double_int16, 1e-15);
  asserts.isequal(im2double([1 + 2i 0]), fixtures.im2double_complex_double);
  asserts.isequal(issparse(im2double(sparse([1 0; 0 2]))), issparse(fixtures.im2double_sparse_double));
  asserts.isequal(full(im2double(sparse([1 0; 0 2]))), full(fixtures.im2double_sparse_double));
  asserts.isequal(im2double(uint8([0 1 255]), 'indexed'), fixtures.im2double_indexed_uint8);
  asserts.isequal(im2double(single([1 2 256]), 'indexed'), fixtures.im2double_indexed_single);
end
%=============================================================================
I = reshape(uint8(linspace(1, 255, 100)), [10 10]);
J = im2double(I);
REF = double(I) / 255;
asserts.isequal(J, REF);

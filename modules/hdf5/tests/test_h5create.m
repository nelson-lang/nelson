%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('h5create'), -4);
asserts.isequal(nargout('h5create'), 0);
%=============================================================================
h5filename = [tempname(), '.h5'];
if isfile(h5filename) rmfile(h5filename) end
%=============================================================================
h5create(h5filename, '/myDataset1', [100 200]);
R = h5read(h5filename, '/myDataset1');
REF = zeros(100, 200);
asserts.isequal(R, REF);
%=============================================================================
h5create(h5filename, '/myDataset2', [100 200], 'FillValue', 3);
R = h5read(h5filename, '/myDataset2');
REF = ones(100, 200) * 3;
asserts.isequal(R, REF);
%=============================================================================
h5create(h5filename, '/myDataset3', [1000 2000], 'Datatype', 'single', 'ChunkSize', [50 80], 'Deflate', 9)
R = h5read(h5filename, '/myDataset3');
REF = single(zeros(1000, 2000));
asserts.isequal(R, REF);
%=============================================================================
h5create(h5filename, '/myDataset4', [200 Inf], 'ChunkSize', [20 20])
%=============================================================================
cmd = 'h5create(h5filename, ''/myDataset4'',[200 Inf], ''ChunkSize'')';
asserts.checkerror(cmd, _('Wrong number of input arguments.'));
%=============================================================================
cmd = 'h5create(h5filename,''/myDataset1'');';
asserts.checkerror(cmd, _('Not enough input arguments.'));
%=============================================================================
if isfile(h5filename) rmfile(h5filename) end
%=============================================================================

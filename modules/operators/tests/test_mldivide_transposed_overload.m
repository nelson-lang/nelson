%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A' \ B still calls the ctranspose method of a class.
folder = [tempdir(), 'test_mldivide_transposed_overload'];
if isdir(folder)
  rmdir(folder, 's');
end
mkdir(folder);
code = {'classdef TransposeProbe', ...
  '  properties', ...
  '    M', ...
  '  end', ...
  '  methods', ...
  '    function obj = TransposeProbe(m)', ...
  '      obj.M = m;', ...
  '    end', ...
  '    function r = ctranspose(obj)', ...
  '      r = obj.M'' + 1;', ...
  '    end', ...
  '  end', ...
  'end'};
fid = fopen([folder, '/TransposeProbe.m'], 'wt');
fprintf(fid, '%s\n', code{:});
fclose(fid);
addpath(folder);
M = [4 1 0; 1 5 2; 0 2 6];
b = [1; 2; 3];
T = TransposeProbe(M);
x = T' \ b;
E = M' + 1;
asserts.isequal(x, E \ b);
rmpath(folder);
rmdir(folder, 's');
%=============================================================================

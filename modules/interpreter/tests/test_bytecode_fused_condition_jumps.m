%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% `if nargin CMP K`, `if ~isa(x, 'cls')` and `if ~isscalar(x)` compile to
% dedicated conditional jumps; the observable behavior must stay identical
% to the generic condition evaluation.
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
%=============================================================================
filewrite([path_test, '/fusedNarginProbe.m'], ...
  ["function r = fusedNarginProbe(a, b, c)"; ...
  "  if nargin < 1"; "    r = -1;"; "    return"; "  end"; ...
  "  if nargin == 1"; "    r = a;"; "    return"; "  end"; ...
  "  if nargin <= 2"; "    r = a + b;"; "    return"; "  end"; ...
  "  if nargin > 2"; "    r = a + b + c;"; "    return"; "  end"; ...
  "  r = -2;"; "end"]);
filewrite([path_test, '/fusedIsaProbe.m'], ...
  ["function x = fusedIsaProbe(x)"; ...
  "  if ~isa(x, 'double')"; "    x = double(x);"; "  end"; "end"]);
filewrite([path_test, '/fusedIsaPositiveProbe.m'], ...
  ["function r = fusedIsaPositiveProbe(x)"; ...
  "  r = 'other';"; ...
  "  if isa(x, 'int32')"; "    r = 'int32';"; "  end"; "end"]);
filewrite([path_test, '/fusedIsscalarProbe.m'], ...
  ["function r = fusedIsscalarProbe(x)"; ...
  "  if ~isscalar(x)"; "    r = 'array';"; "  else"; "    r = 'scalar';"; ...
  "  end"; "end"]);
filewrite([path_test, '/fusedShadowedProbe.m'], ...
  ["function r = fusedShadowedProbe(k)"; ...
  "  isscalar = [10 20 30];"; ...
  "  if isscalar(k) > 15"; "    r = 'big';"; "  else"; "    r = 'small';"; ...
  "  end"; "end"]);
filewrite([path_test, '/fusedUndefinedProbe.m'], ...
  ["function r = fusedUndefinedProbe()"; ...
  "  if ~isscalar(neverAssignedVariable)"; "    r = 1;"; ...
  "  else"; "    r = 2;"; "  end"; "end"]);
filewrite([path_test, '/fusedArgumentsProbe.m'], ...
  ["function y = fusedArgumentsProbe(a, b)"; ...
  "  arguments"; "    a (1,1) double"; "    b (1,1) double = 41"; "  end"; ...
  "  y = a + b;"; "end"]);
addpath(path_test);
%=============================================================================
asserts.isequal(fusedNarginProbe(), -1);
asserts.isequal(fusedNarginProbe(5), 5);
asserts.isequal(fusedNarginProbe(5, 6), 11);
asserts.isequal(fusedNarginProbe(5, 6, 7), 18);
%=============================================================================
asserts.isequal(class(fusedIsaProbe(int32(5))), 'double');
asserts.isequal(class(fusedIsaProbe(2.5)), 'double');
asserts.isequal(fusedIsaProbe('a'), 97);
%=============================================================================
% positive polarity
asserts.isequal(fusedIsaPositiveProbe(int32(1)), 'int32');
asserts.isequal(fusedIsaPositiveProbe(1), 'other');
%=============================================================================
asserts.isequal(fusedIsscalarProbe(5), 'scalar');
asserts.isequal(fusedIsscalarProbe([1 2]), 'array');
asserts.isequal(fusedIsscalarProbe([]), 'array');
%=============================================================================
% a local variable named like the predicate disables the fused check
asserts.isequal(fusedShadowedProbe(1), 'small');
asserts.isequal(fusedShadowedProbe(3), 'big');
%=============================================================================
% undefined base keeps erroring through the generic window
undefinedErrorSeen = false;
try
  fusedUndefinedProbe();
catch err
  undefinedErrorSeen = contains(err.message, 'neverAssignedVariable');
end
asserts.istrue(undefinedErrorSeen);
%=============================================================================
% arguments blocks keep converting, defaulting and rejecting
asserts.isequal(fusedArgumentsProbe(1), 42);
asserts.isequal(fusedArgumentsProbe(1, 2), 3);
asserts.isequal(fusedArgumentsProbe(int32(1)), 42);
wrongSizeSeen = false;
try
  fusedArgumentsProbe([1 2]);
catch
  wrongSizeSeen = true;
end
asserts.istrue(wrongSizeSeen);
wrongCountSeen = false;
try
  fusedArgumentsProbe();
catch
  wrongCountSeen = true;
end
asserts.istrue(wrongCountSeen);
%=============================================================================
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================

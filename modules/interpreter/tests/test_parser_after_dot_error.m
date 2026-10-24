%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A parse that stops on a '.' token must not make the next parse read its
% first keyword as a field name (the stale token also broke, at random, the
% generated plus forwarder of classdef subclasses of builtin types).
%=============================================================================
badSources = {'x = .', '.', 'x = 1 . 2', 'x..y', '[1 .]', 'x = (.)'};
goodSources = {'if true, v = 1; end', ...
  'for k = 1:3, v = v + k; end', ...
  'switch 2, case 2, v = 4; end', ...
  'try, v = 5; catch, end'};
expected = [1, 6, 4, 5];
for i = 1:numel(badSources)
  for j = 1:numel(goodSources)
    failed = false;
    try
      eval(badSources{i});
    catch
      failed = true;
    end
    asserts.istrue(failed);
    v = 0;
    eval(goodSources{j});
    asserts.isequal(v, expected(j));
  end
end
%=============================================================================

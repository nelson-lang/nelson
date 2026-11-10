%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% an object argument reaches the class method of accumarray, isbetween and allbetween
d = [tempdir(), 'accum_overload_', strrep(num2str(now() * 1e10), '.', '')];
mkdir(d);
lines = {'classdef accumOverloadProbe', ...
         '  methods', ...
         '    function r = accumarray(varargin)', ...
         '      r = ''accumarray method'';', ...
         '    end', ...
         '    function r = isbetween(varargin)', ...
         '      r = ''isbetween method'';', ...
         '    end', ...
         '    function r = allbetween(varargin)', ...
         '      r = ''allbetween method'';', ...
         '    end', ...
         '  end', ...
         'end'};
writelines(lines, [d, '/accumOverloadProbe.m']);
addpath(d);
cleanup = onCleanup(@() rmpath(d));
p = accumOverloadProbe();
assert_isequal(accumarray(p, [1; 2]), 'accumarray method');
assert_isequal(accumarray([1; 2], p), 'accumarray method');
assert_isequal(isbetween(p, 1, 3), 'isbetween method');
assert_isequal(isbetween(2, p, 3), 'isbetween method');
assert_isequal(allbetween(p, 1, 3), 'allbetween method');
%=============================================================================
% plain values still use the built-in code
assert_isequal(accumarray([1; 2; 1], [1; 2; 3], [], @max), [3; 2]);
assert_istrue(isbetween(2, 1, 3));
assert_istrue(allbetween([1 2 3], 0, 4));
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% Ranges with literal bounds are built once: same values, shapes and class as
% the colon operator, and a variable holding one is still copied on write.
%=============================================================================
r = literal_ranges();
assert_isequal(r{1}, [1 2 3 4 5]);
assert_isequal(r{2}, [-2 -1 0 1 2]);
assert_isequal(r{3}, [0 0.25 0.5 0.75 1]);
assert_isequal(r{4}, [5 4 3 2 1]);
assert_isequal(size(r{5}), [1 0]);
assert_isequal(class(r{5}), 'double');
assert_istrue(isnan(r{6}));
assert_isequal(r{7}, colon(1, 2000));
assert_isequal(r{8}, colon(1, 0.1, 1.3));
assert_isequal(r{9}, colon(-1.5, 0.5, 1));
%=============================================================================
m = modified_ranges();
assert_isequal(m{1}, [1 7 3 4 5]);
assert_isequal(m{2}, [1 2 3 4 5 6]);
assert_isequal(m{3}, [1 2 3 4 5 9]);
assert_isequal(m{4}, [1 2 3 4 5]);
%=============================================================================
% an invalid range still fails when it runs
assert_checkerror('literal_infinite_range()', 'Invalid range.');
%=============================================================================
function r = literal_ranges()
  r = {};
  for k = 1:2
    r = {1:5, -2:2, 0:0.25:1, 5:-1:1, 5:1, 1:NaN, 1:2000, 1:0.1:1.3, -1.5:0.5:1};
  end
end
%=============================================================================
function r = modified_ranges()
  r = {};
  for k = 1:3
    v = 1:5;
    w = 1:5;
    u = 1:5;
    v(2) = 7;
    w(end + 1) = 6; %#ok<NLS0039>
    u = [u, 9]; %#ok<NLS0039>
    r = {v, w, u, 1:5};
  end
end
%=============================================================================
function r = literal_infinite_range() %#ok<NLS0023>
  r = 1:Inf;
end
%=============================================================================

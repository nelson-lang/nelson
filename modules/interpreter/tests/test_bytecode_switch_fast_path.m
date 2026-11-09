%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% switch on text or numbers in a loop picks the same case as a single switch:
% every character compared, numbers compared by value, NaN never matches.
%=============================================================================
assert_isequal(loop_switch('beta'), 2);
assert_isequal(loop_switch('alpha'), 1);
assert_isequal(loop_switch('alphabet'), 4);
assert_isequal(loop_switch("beta"), 2);
assert_isequal(loop_switch('gamma'), 3);
assert_isequal(loop_switch(['a', char(0), 'b']), 4);
assert_isequal(loop_switch(['a', char(0), 'c']), 5);
assert_isequal(loop_switch(2), 6);
assert_isequal(loop_switch(int8(2)), 6);
assert_isequal(loop_switch(1), 7);
assert_isequal(loop_switch(NaN), 4);
assert_isequal(loop_switch(3.5), 4);
assert_isequal(loop_switch('2'), 4);
%=============================================================================
assert_checkerror('loop_switch([1 2])', 'Switch argument must be a scalar or a string');
%=============================================================================
function r = loop_switch(value) %#ok<NLS0026>
  r = 0;
  for k = 1:2
    switch value
      case 'alpha'
        r = 1;
      case 'beta'
        r = 2;
      case {'gamma', 'delta'}
        r = 3;
      case ['a', char(0), 'c']
        r = 5;
      case 2
        r = 6;
      case 1
        r = 7;
      case NaN
        r = 8;
      otherwise
        r = 4;
    end
  end
end
%=============================================================================

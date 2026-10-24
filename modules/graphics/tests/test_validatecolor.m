%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
R = validatecolor(uint8([128 0 255]));
REF = [0.5020 0 1.0000];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
c = uint16([32768 0 65535; 0 65535 0]);
R = validatecolor(c, 'multiple');
REF = [0.5000 0 1.0000; 0 1.0000 0];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = validatecolor({'red', 'green', 'blue'}, 'multiple');
REF = eye(3, 3);
asserts.isequal(R, REF);
%=============================================================================
R = validatecolor({'#8000FF', '#0F0000', '#FF9900'}, 'multiple');
REF = [0.5020 0 1.0000; 0.0588 0 0; 1.0000 0.6000 0];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = validatecolor(["#8000FF", "#0F0000", "#FF9900"], 'multiple');
REF = [0.5020 0 1.0000; 0.0588 0 0; 1.0000 0.6000 0];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = validatecolor('#F80');
REF = [1 0.5333 0];
asserts.isapprox(R, REF, 1e-4);
R = validatecolor({'#f80', '#FF8800', '#000'}, 'multiple');
REF = [1 0.5333 0; 1 0.5333 0; 0 0 0];
asserts.isapprox(R, REF, 1e-4);
assert_checkerror('validatecolor(''#F8'')', getString(message('Nelson:graphics:validatecolor:InvalidColor')), 'Nelson:graphics:validatecolor:InvalidColor');
%=============================================================================
% hexadecimal codes give exact k/255 values
assert_isequal(validatecolor('#336699'), [51 102 153] / 255);
assert_isequal(validatecolor({'#010203', '#FEFDFC'}, 'multiple'), [1 2 3; 254 253 252] / 255);
%=============================================================================
% text colors must be a 1-D cell array or string array
msg = getString(message('Nelson:graphics:validatecolor:InvalidStringShape'));
assert_checkerror('validatecolor({''red'', ''blue''; ''green'', ''#F80''}, ''multiple'')', msg);
assert_checkerror('validatecolor({''red'', ''blue''; ''green'', ''#F80''}, ''multiple'')', msg, 'Nelson:graphics:validatecolor:InvalidStringShape');
assert_checkerror('validatecolor(["red", "blue"; "green", "#F80"], ''multiple'')', msg, 'Nelson:graphics:validatecolor:InvalidStringShape');
assert_checkerror('validatecolor({''red'', ''blue''; ''green'', ''#F80''})', getString(message('Nelson:graphics:validatecolor:MultipleColors')), 'Nelson:graphics:validatecolor:MultipleColors');
assert_isequal(validatecolor({'red'; 'blue'}, 'multiple'), [1 0 0; 0 0 1]);
assert_isequal(validatecolor({}, 'multiple'), zeros(0, 3));
%=============================================================================
% a cell that is not made of character vectors, or an unsupported class
assert_checkerror('validatecolor({1, 2, 3})', getString(message('Nelson:graphics:validatecolor:InvalidColorCell')), 'Nelson:graphics:validatecolor:InvalidColorCell');
assert_checkerror('validatecolor(struct())', getString(message('Nelson:graphics:validatecolor:InvalidColorType')), 'Nelson:graphics:validatecolor:InvalidColorType');
%=============================================================================

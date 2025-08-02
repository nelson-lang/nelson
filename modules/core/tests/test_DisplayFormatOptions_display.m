%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = format();
%=============================================================================
R = evalc('display(f)');
REF = '
f =

  DisplayFormatOptions with properties:

    NumericFormat: "short"
      LineSpacing: "loose"

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('disp(f)');
REF = '    NumericFormat: "short"
LineSpacing: "loose"

';
%=============================================================================

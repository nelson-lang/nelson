%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
obj = ClassdefLocalFunctions(3);
asserts.isequal(class(obj), 'ClassdefLocalFunctions');
asserts.isequal(obj.ValueStorage, 4);
asserts.isequal(obj.Value, 8);
asserts.isequal(obj.combine(4), 9);
asserts.isequal(obj.privateLocalValue(), 15);
st = struct(obj);
asserts.isequal(st.Value, 8);
asserts.isequal(st.Combined, 5);
%=============================================================================
obj.Value = 6;
asserts.isequal(obj.ValueStorage, 7);
asserts.isequal(obj.Value, 14);
asserts.isequal(obj.privateLocalValue(), 18);
%=============================================================================
storedPropertySetter = ClassdefStoredPropertySetter(3);
asserts.isequal(storedPropertySetter.Value, 6);
storedPropertySetter.Value = 4;
asserts.isequal(storedPropertySetter.Value, 8);
try
  ClassdefStoredPropertySetter(Inf);
  error('stored property setter should reject non-finite values');
catch M
  asserts.isequal(M.message, 'Stored value must be finite.');
end
%=============================================================================
obj = ClassdefLocalFunctions();
asserts.isequal(obj.ValueStorage, 1);
asserts.isequal(obj.Value, 2);
%=============================================================================
oneLine = ClassdefOneLineMethods();
asserts.isequal(oneLine.Value, 3);
asserts.isequal(oneLine.value(), 4);
asserts.isequal(ClassdefOneLineMethods.staticValue(), 9);
asserts.isfalse(ismethod(oneLine, 'privateValue'));
%=============================================================================
rmpath(path_test);
%=============================================================================

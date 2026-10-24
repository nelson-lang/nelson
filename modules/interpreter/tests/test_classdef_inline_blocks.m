%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A property / event / enumeration block may be written fully inline on one
% physical line ("properties; X = 1; Y = 2; end"). The line-based scanner
% assumed one construct per line and rejected it. An inline-block expansion pass
% now splits such a line into its equivalent multi-line form before scanning, so
% these compact classes load correctly.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
obj = ClassdefInlineBlocks();
asserts.isequal(obj.A, 10);
asserts.isequal(obj.B, 20);
asserts.isequal(obj.total(), 30);
asserts.istrue(any(strcmp(events(obj), 'Changed')));
asserts.istrue(any(strcmp(events(obj), 'Updated')));
%=============================================================================
asserts.isequal(int32(ClassdefInlineEnum.Red), int32(1));
asserts.isequal(int32(ClassdefInlineEnum.Green), int32(2));
asserts.isequal(int32(ClassdefInlineEnum.Blue), int32(3));
%=============================================================================
rmpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
correction = nelson.lang.correction.ConvertToFunctionNotationCorrection('plot');
%=============================================================================
asserts.isequal(class(correction), ...
  'nelson.lang.correction.ConvertToFunctionNotationCorrection');
asserts.isequal(correction.Method, 'plot');
asserts.checkerror('correction.Method = ''mesh'';', ...
  _('Cannot set property: Method'));
%=============================================================================

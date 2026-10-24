%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/missing_input_argument/']);
%=============================================================================
% reading a formal input parameter that the caller did not pass
msg = _('Not enough input arguments.');
id = 'Nelson:notEnoughInputs';
asserts.checkerror('missing_input_plus_one()', msg, id);
asserts.isequal(missing_input_plus_one(1), 2);
for mode = 1:6
  asserts.checkerror(sprintf('missing_input_second(%d)', mode), msg, id);
end
asserts.isequal(missing_input_second(7), 7);
asserts.isequal(missing_input_second(1, 4), 4);
asserts.isequal(missing_input_second(3, {5}), 5);
%=============================================================================
% parameters assigned under a nargin guard and varargin are defined
asserts.isequal(missing_input_default(1), 11);
asserts.isequal(missing_input_default(1, 2), 3);
asserts.isequal(missing_input_varargin(1), 1);
asserts.checkerror('missing_input_varargin()', msg, id);
%=============================================================================
% other undefined variables keep the undefined-variable error
asserts.checkerror('missing_input_cleared(1)', sprintf(_('Undefined variable: %s'), 'x'), 'Nelson:UndefinedVariable');
asserts.checkerror('missing_input_local(-1)', sprintf(_('Undefined variable: %s'), 'z'), 'Nelson:UndefinedVariable');
%=============================================================================
% classdef constructor called without its input, directly or through an
% implicit superclass constructor call
asserts.checkerror('MissingInputCtorBase()', msg, id);
asserts.checkerror('MissingInputCtorChild()', msg, id);
asserts.checkerror('MissingInputCtorExplicitChild(1)', msg, id);
obj = MissingInputCtorBase(4);
asserts.isequal(obj.V, 4);
%=============================================================================

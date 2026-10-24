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
[T] = graphics_stackedplot_fixture();
s = stackedplot(T, ["Temperature", "Rain"], XVariable = "Time", GridVisible = "on");
asserts.isequal(s.DisplayVariables, {'Temperature', 'Rain'});
asserts.isequal(s.XVariable, 'Time');
asserts.isequal(char(s.GridVisible), 'on');
close all;
%=============================================================================

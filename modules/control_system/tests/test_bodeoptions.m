%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opt = bodeoptions();
asserts.isequal(class(opt), 'struct');
asserts.isequal(opt.PhaseUnits, 'deg');
asserts.isequal(opt.Grid, 'off');
asserts.isequal(opt.FreqUnits, 'rad/s');
asserts.isequal(opt.MagUnits, 'dB');
asserts.istrue(isstruct(opt.Title));
asserts.istrue(isstruct(opt.XLabel));
asserts.istrue(isstruct(opt.YLabel));
%=============================================================================
opt = bodeoptions();
opt.PhaseUnits = 'rad';
opt.Grid = 'on';
asserts.isequal(opt.PhaseUnits, 'rad');
asserts.isequal(opt.Grid, 'on');
%=============================================================================

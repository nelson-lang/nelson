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
new_system('brmodel');
add_block('nflow/math/gain', 'brmodel/Gain');
%=============================================================================
% bdroot returns the top-level model name of a path.
asserts.isequal(bdroot('brmodel'), 'brmodel');
asserts.isequal(bdroot('brmodel/Gain'), 'brmodel');
%=============================================================================
% The root model must be loaded.
asserts.checkerror('bdroot(''nosuchmodel/Gain'')', _('Model not loaded: nosuchmodel'));
%=============================================================================
bdclose('brmodel');
%=============================================================================

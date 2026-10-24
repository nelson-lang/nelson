%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
correction = nelson.lang.correction.ReplaceIdentifierCorrection('oldName', 'newName');
%=============================================================================
asserts.isequal(class(correction), 'nelson.lang.correction.ReplaceIdentifierCorrection');
asserts.isequal(correction.Identifier, 'oldName');
asserts.isequal(correction.Replacement, 'newName');
asserts.checkerror('correction.Identifier = ''other'';', ...
  _('Cannot set property: Identifier'));
%=============================================================================

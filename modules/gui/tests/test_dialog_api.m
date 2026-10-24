%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
asserts.isequal(nargin('dialog'), -1);
asserts.isequal(nargout('dialog'), 1);
asserts.isequal(nargin('inputdlg'), 5);
asserts.isequal(nargout('inputdlg'), 1);
asserts.isequal(nargin('listdlg'), -1);
asserts.isequal(nargout('listdlg'), 2);
asserts.isequal(nargin('uigetfile'), -1);
asserts.isequal(nargout('uigetfile'), 3);
asserts.isequal(nargin('uiputfile'), -1);
asserts.isequal(nargout('uiputfile'), 3);
asserts.isequal(nargin('uigetdir'), -1);
asserts.isequal(nargout('uigetdir'), 1);
asserts.isequal(nargin('uiopen'), 2);
asserts.isequal(nargout('uiopen'), 0);
asserts.isequal(nargin('uisave'), 2);
asserts.isequal(nargout('uisave'), 0);
asserts.isequal(nargin('uisetcolor'), -1);
asserts.isequal(nargout('uisetcolor'), 1);
asserts.isequal(nargin('uisetfont'), -1);
asserts.isequal(nargout('uisetfont'), 1);
asserts.isequal(nargin('uifigure'), -1);
asserts.isequal(nargout('uifigure'), 1);
asserts.isequal(nargin('waitbar'), -3);
asserts.isequal(nargout('waitbar'), 1);
asserts.isequal(nargin('uialert'), -4);
asserts.isequal(nargout('uialert'), 0);
asserts.isequal(nargin('uiconfirm'), -4);
asserts.isequal(nargout('uiconfirm'), 1);
asserts.isequal(nargin('uiprogressdlg'), -2);
asserts.isequal(nargout('uiprogressdlg'), 1);
%=============================================================================
asserts.checkerror('inputdlg()', _('Not enough input arguments.'));
asserts.checkerror('uiopen(1, 2, 3)', _('Too many input arguments.'));
asserts.checkerror('uisave(1, 2, 3)', _('Too many input arguments.'));
%=============================================================================

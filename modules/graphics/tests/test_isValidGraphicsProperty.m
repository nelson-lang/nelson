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
asserts.istrue(isValidGraphicsProperty('axes', 'Type'));
asserts.istrue(isValidGraphicsProperty('polaraxes', 'Type'));
asserts.istrue(isValidGraphicsProperty('polaraxes', 'Color'));
asserts.istrue(isValidGraphicsProperty('polaraxes', 'RLim'));
asserts.istrue(isValidGraphicsProperty('polaraxes', 'ThetaAxisUnits'));
asserts.istrue(isValidGraphicsProperty('polaraxes', 'ThetaTickLabel'));
asserts.istrue(isValidGraphicsProperty('line', 'Type'));
asserts.istrue(isValidGraphicsProperty('image', 'Type'));
asserts.istrue(isValidGraphicsProperty('root', 'Type'));
asserts.istrue(isValidGraphicsProperty('text', 'Type'));
asserts.istrue(isValidGraphicsProperty('figure', 'Type'));
asserts.istrue(isValidGraphicsProperty('uicontextmenu', 'ContextMenuOpeningFcn'));
asserts.istrue(isValidGraphicsProperty('uimenu', 'MenuSelectedFcn'));
asserts.istrue(isValidGraphicsProperty('uimenu', 'Text'));
asserts.istrue(isValidGraphicsProperty('uimenu', 'Accelerator'));
asserts.istrue(isValidGraphicsProperty('arrow', 'HeadStyle'));
asserts.istrue(isValidGraphicsProperty('doublearrow', 'Head2Width'));
asserts.istrue(isValidGraphicsProperty('textarrow', 'TextRotation'));
asserts.istrue(isValidGraphicsProperty('rectangle', 'Rotation'));
asserts.istrue(isValidGraphicsProperty('ellipse', 'Rotation'));
asserts.istrue(isValidGraphicsProperty('textbox', 'FitBoxToText'));
%=============================================================================
asserts.isfalse(isValidGraphicsProperty('axes', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('axes', 'RLim'));
asserts.isfalse(isValidGraphicsProperty('axes', 'ThetaTickLabel'));
asserts.isfalse(isValidGraphicsProperty('polaraxes', 'XLim'));
asserts.isfalse(isValidGraphicsProperty('polaraxes', 'XTick'));
asserts.isfalse(isValidGraphicsProperty('polaraxes', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('line', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('image', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('root', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('text', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('figure', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('uicontextmenu', 'Text'));
asserts.isfalse(isValidGraphicsProperty('uimenu', 'String'));
asserts.isfalse(isValidGraphicsProperty('uimenu', 'TypeA'));
asserts.isfalse(isValidGraphicsProperty('arrow', 'FaceAlpha'));
asserts.isfalse(isValidGraphicsProperty('ellipse', 'FaceAlpha'));
%=============================================================================

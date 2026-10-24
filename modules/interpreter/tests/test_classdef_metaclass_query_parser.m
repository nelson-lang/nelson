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
asserts.isequal(parsestring('m = ?ClassdefPoint;'), 'script');
asserts.isequal(parsestring('m = ?classdefpkg.ClassdefPackaged;'), 'script');
asserts.isequal(parsestring('c = {?ClassdefPoint, ?ClassdefColor};'), 'script');
asserts.isequal(parsestring('x = [1 2 3]''; m = ?ClassdefPoint;'), 'script');
asserts.isequal(parsestring('txt = ''<?xml version="1.0"?>'';'), 'script');
asserts.isequal(parsestring('txt = ''?ClassdefPoint'';'), 'script');
asserts.isequal(parsestring('txt = "?ClassdefPoint";'), 'script');
asserts.isequal(parsestring(sprintf('%% ?ClassdefPoint\nx = 1;')), 'script');
asserts.isequal(parsestring(sprintf('%%{\n?ClassdefPoint\n%%}\nx = 1;')), 'script');
asserts.isequal(parsestring('x = a?;'), 'error');
%=============================================================================
pointMeta = ?ClassdefPoint;
asserts.isequal(pointMeta.Name, 'ClassdefPoint');
packageMeta = ?classdefpkg.ClassdefPackaged;
asserts.isequal(packageMeta.Name, 'classdefpkg.ClassdefPackaged');
metaCells = {?ClassdefPoint, ?ClassdefColor};
asserts.isequal(metaCells{1}.Name, 'ClassdefPoint');
asserts.isequal(metaCells{2}.Name, 'ClassdefColor');
functionMeta = useClassdefPointMetaQuery();
asserts.isequal(functionMeta.Name, 'ClassdefPoint');
%=============================================================================
columnValues = [1 2 3]';
asserts.isequal(columnValues, [1; 2; 3]);
xmlText = '<?xml version="1.0"?>';
asserts.isequal(xmlText, '<?xml version="1.0"?>');
charText = '?ClassdefPoint';
asserts.isequal(charText, '?ClassdefPoint');
stringText = "?ClassdefPoint";
asserts.isequal(stringText, "?ClassdefPoint");
%=============================================================================
rmpath(path_test);
%=============================================================================

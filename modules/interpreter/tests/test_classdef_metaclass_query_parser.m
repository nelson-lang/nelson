%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
assert_isequal(parsestring('m = ?ClassdefPoint;'), 'script');
assert_isequal(parsestring('m = ?classdefpkg.ClassdefPackaged;'), 'script');
assert_isequal(parsestring('c = {?ClassdefPoint, ?ClassdefColor};'), 'script');
assert_isequal(parsestring('x = [1 2 3]''; m = ?ClassdefPoint;'), 'script');
assert_isequal(parsestring('txt = ''<?xml version="1.0"?>'';'), 'script');
assert_isequal(parsestring('txt = ''?ClassdefPoint'';'), 'script');
assert_isequal(parsestring('txt = "?ClassdefPoint";'), 'script');
assert_isequal(parsestring(sprintf('%% ?ClassdefPoint\nx = 1;')), 'script');
assert_isequal(parsestring(sprintf('%%{\n?ClassdefPoint\n%%}\nx = 1;')), 'script');
assert_isequal(parsestring('x = a?;'), 'error');
%=============================================================================
pointMeta = ?ClassdefPoint;
assert_isequal(pointMeta.Name, 'ClassdefPoint');
packageMeta = ?classdefpkg.ClassdefPackaged;
assert_isequal(packageMeta.Name, 'classdefpkg.ClassdefPackaged');
metaCells = {?ClassdefPoint, ?ClassdefColor};
assert_isequal(metaCells{1}.Name, 'ClassdefPoint');
assert_isequal(metaCells{2}.Name, 'ClassdefColor');
functionMeta = useClassdefPointMetaQuery();
assert_isequal(functionMeta.Name, 'ClassdefPoint');
%=============================================================================
columnValues = [1 2 3]';
assert_isequal(columnValues, [1; 2; 3]);
xmlText = '<?xml version="1.0"?>';
assert_isequal(xmlText, '<?xml version="1.0"?>');
charText = '?ClassdefPoint';
assert_isequal(charText, '?ClassdefPoint');
stringText = "?ClassdefPoint";
assert_isequal(stringText, "?ClassdefPoint");
%=============================================================================
rmpath(path_test);
%=============================================================================

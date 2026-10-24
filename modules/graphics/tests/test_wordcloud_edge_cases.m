%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
h = wordcloud(f, {'a', 'b'}, [2 1], 'MaxDisplayWords', 5);
texts = findall(f, 'Type', 'text');
asserts.isequal(numel(texts), 2);
asserts.isequal(h.MaxDisplayWords, 5);
asserts.isequal(cellstr(h.WordData), {'a', 'b'});
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
h = wordcloud(f, {'a', 'b', 'c'}, [1 1 1], 'Shape', 'rectangle');
texts = findall(f, 'Type', 'text');
aText = texts(strcmp({texts.String}, 'a'));
bText = texts(strcmp({texts.String}, 'b'));
cText = texts(strcmp({texts.String}, 'c'));
asserts.isequal(numel(texts), 3);
asserts.isequal(aText.FontSize, 87);
asserts.isequal(bText.FontSize, 87);
asserts.isequal(cText.FontSize, 87);
asserts.isequal(h.Shape, 'rectangle');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
c = categorical({'b', 'a', 'b', 'c', 'a', 'b'});
h = wordcloud(f, c, 'MaxDisplayWords', 2);
texts = findall(f, 'Type', 'text');
asserts.isequal(numel(texts), 2);
asserts.isequal(cellstr(h.WordData), {'b', 'a', 'c'});
asserts.isequal(h.SizeData, [3 2 1]);
asserts.isequal(h.MaxDisplayWords, 2);
close(f);
%=============================================================================
asserts.checkerror('wordcloud({''a'', ''b''}, [2 1], ''MaxDisplayWords'', 0);', getString(message('Nelson:graphics:mustBePositiveScalar', 'MaxDisplayWords')));
asserts.checkerror('wordcloud({''a'', ''b''}, [2 1], ''MaxDisplayWords'', -1);', getString(message('Nelson:graphics:mustBePositiveScalar', 'MaxDisplayWords')));
asserts.checkerror('wordcloud({''a'', ''b''}, [2 1], ''MaxDisplayWords'', [1 2]);', getString(message('Nelson:graphics:mustBePositiveScalar', 'MaxDisplayWords')));
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('getavailablelanguages'), 0);
asserts.isequal(nargout('getavailablelanguages'), 1);
%=============================================================================
languages = getavailablelanguages();
asserts.isequal(class(languages), 'cell');
asserts.istrue(~isempty(languages));
localeDirectory = fullfile(nelsonroot(), 'locale');
asserts.istrue(isfolder(localeDirectory));
asserts.istrue(isfile(fullfile(localeDirectory, ['nelson-errors-', getdefaultlanguage(), '.json'])));
for k = 1:numel(languages)
  asserts.isequal(class(languages{k}), 'char');
  asserts.istrue(~isempty(languages{k}));
  asserts.istrue(isfile(fullfile(localeDirectory, ['nelson-ui-', languages{k}, '.json'])));
end
%=============================================================================

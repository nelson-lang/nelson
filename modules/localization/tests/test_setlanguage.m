%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('setlanguage'), 1);
asserts.isequal(nargout('setlanguage'), 1);
%=============================================================================
initialLanguage = getlanguage();
languages = getavailablelanguages();
targetLanguage = languages{1};
for k = 1:length(languages)
  if ~strcmp(languages{k}, initialLanguage)
    targetLanguage = languages{k};
    break
  end
end
try
  status = setlanguage(targetLanguage);
  asserts.istrue(islogical(status));
  asserts.isequal(getlanguage(), targetLanguage);
  status = setlanguage(initialLanguage);
  asserts.istrue(islogical(status));
  asserts.isequal(getlanguage(), initialLanguage);
catch ex
  setlanguage(initialLanguage);
  rethrow(ex);
end
%=============================================================================

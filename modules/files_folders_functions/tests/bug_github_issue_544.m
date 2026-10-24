%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/544
% <-- Short Description -->
% add 'folder' fieldname to 'dir' output
%=============================================================================
st = dir(nelsonroot());
names = fieldnames(st);
REF = {'name';
  'folder';
  'date';
  'bytes';
  'isdir';
  'datenum'};
asserts.isequal(names, REF);
idx = find(strcmp({st.name}, '.'), 1);
if isempty(idx)
  idx = find(strcmp({st.folder}, nelsonroot()) & [st.isdir], 1);
end
asserts.istrue(~isempty(idx))
asserts.isequal(st(idx).folder, nelsonroot())
asserts.isequal(st(idx).isdir, true)
%=============================================================================

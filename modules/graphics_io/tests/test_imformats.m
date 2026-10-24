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
imformats()
%=============================================================================
st = imformats();
asserts.istrue(isstruct(st));
asserts.istrue(length(st) > 5);
%=============================================================================
imfmt = imformats('png');
names = fieldnames(imfmt);
asserts.istrue(length(names) > 0);
asserts.istrue(ismember('ext', names));
asserts.istrue(ismember('isa', names));
asserts.istrue(ismember('info', names));
asserts.istrue(ismember('read', names));
asserts.istrue(ismember('write', names));
asserts.istrue(ismember('alpha', names));
asserts.istrue(ismember('description', names));
asserts.istrue(ismember('multipage', names));
%=============================================================================
imfmt = imformats('png44');
asserts.istrue(isempty(imfmt));
%=============================================================================

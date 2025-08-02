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
assert_istrue(isstruct(st));
assert_istrue(length(st) > 5);
%=============================================================================
imfmt = imformats('png');
names = fieldnames(imfmt);
assert_istrue(length(names) > 0);
assert_istrue(ismember('ext', names));
assert_istrue(ismember('isa', names));
assert_istrue(ismember('info', names));
assert_istrue(ismember('read', names));
assert_istrue(ismember('write', names));
assert_istrue(ismember('alpha', names));
assert_istrue(ismember('description', names));
assert_istrue(ismember('multipage', names));
%=============================================================================
imfmt = imformats('png44');
assert_istrue(isempty(imfmt));
%=============================================================================

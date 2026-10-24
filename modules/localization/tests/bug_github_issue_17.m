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
% https://github.com/nelson-lang/nelson/issues/17
% <-- Short Description -->
% 'locales' directory renamed as 'locale' (more standard).
%=============================================================================
% The 'locale' directory (renamed from 'locales', see issue #17) must be
% present. Its location relative to nelsonroot() depends on the install layout:
% a system/Linux install keeps it beside the share tree (root/../locale), while
% a self-contained bundle (macOS .app) keeps it under nelsonroot (root/locale).
% The presence of a lib/ next to bin/ no longer distinguishes these (a macOS
% bundle has both a lib/ and the bundled layout), so accept either location.
locale_root = modulepath('nelson', 'root');
hasLocale = isdir([locale_root, '/locale']) || isdir([locale_root, '/../locale']);
asserts.istrue(hasLocale);
%=============================================================================

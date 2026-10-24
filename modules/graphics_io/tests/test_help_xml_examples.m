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
skip_testsuite(~isfile([modulepath('graphics_io'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
graphics_io_run_help_page('en_US', 'imformats', 3);
%=============================================================================

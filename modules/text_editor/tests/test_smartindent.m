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
% <--CHECK REF-->
copyfile([modulepath('text_editor', 'tests'), '/smartindent.txt'], [tempdir(),'/smartindent.txt']);
smartindent([tempdir(),'/smartindent.txt'])
fileread([tempdir(),'/smartindent.txt'], 'char', 'native')

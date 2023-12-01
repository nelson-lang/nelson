%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
pIE = actxserver('InternetExplorer.Application')
invoke(pIE, 'Navigate', 'https://nelson-lang.github.io/nelson-website/')
pIE.Visible = true
sleep(5)
invoke(pIE,'Quit')
delete(pIE)
clear pIE
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% repo() without any input argument must raise an error, not read out of bounds.
asserts.checkerror('repo()', _('Not enough input arguments.'));
asserts.checkerror('repo()', _('Not enough input arguments.'), 'Nelson:notEnoughInputs');
%=============================================================================
% repo('clone') / repo('export') read the url argument before checking the
% argument count; too few inputs must raise an error, not read out of bounds.
asserts.checkerror('repo(''clone'')', _('Not enough input arguments.'), 'Nelson:notEnoughInputs');
asserts.checkerror('repo(''export'')', _('Not enough input arguments.'), 'Nelson:notEnoughInputs');
%=============================================================================

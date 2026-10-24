%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
destinationdir = tempname();
mkdir(destinationdir);
dlgeneratemexgateway(destinationdir, 'sampleMex', true);
filename = [destinationdir, '/mexGateway.c'];
asserts.istrue(isfile(filename));
txt = fileread(filename);
asserts.istrue(contains(txt, 'sampleMex'));
asserts.istrue(contains(txt, 'MX_CURRENT_API_VER'));
rmdir(destinationdir, 's');
%=============================================================================

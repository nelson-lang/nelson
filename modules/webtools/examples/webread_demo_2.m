%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Use a function_handle with weboptions
% to read an audio and play it directly
current = nfilename('fullpathext');
[p, f, e ] = fileparts(current);
addpath(p);
fh = str2func('download_play');
o = weboptions('ContentType', 'binary', 'ContentReader', fh);
o.Timeout = 10;
R = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', o)
%=============================================================================

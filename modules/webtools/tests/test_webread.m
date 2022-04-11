%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('webread'), -1);
assert_isequal(nargout('webread'), -1);
%=============================================================================
o = weboptions('RequestMethod', 'get');
R = webread('http://httpbin.org/get', o);
assert_istrue(isstruct(R));
assert_isequal(R.url, 'http://httpbin.org/get');
%=============================================================================
o = weboptions('ContentType', 'text');
R = webread('http://httpbin.org/get', o);
assert_istrue(ischar(R));
%=============================================================================
if ismodule('audio')
    o = weboptions('ContentType', 'audio');
    o.Timeout = 60;
    [y, fs] = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', o);
    assert_isequal(size(y), [527617           2]);
    assert_isequal(y(1), 0);
    assert_isequal(fs, 44100);
end
%=============================================================================
if ismodule('audio')
    o = weboptions('ContentType', 'binary');
    o.Timeout = 60;
    R = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', o);
    assert_isequal(size(R), [2110562           1]);
    REF = uint8([   82;   73;   70;   70;   90;   52;   32;    0;   87;   65]);
    assert_isequal(R(1:10), REF(1:10));
end
%=============================================================================
o = weboptions('ContentType', 'raw');
o.Timeout = 60;
R = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', o);
assert_isequal(size(R), [2110562           1]);
REF = uint8([   82;   73;   70;   70;   90;   52;   32;    0;   87;   65]);
assert_isequal(R(1:10), REF(1:10));
%=============================================================================
if ismodule('audio')
    o = weboptions('ContentType', 'binary', 'ContentReader', str2func('audioread'));
    o.Timeout = 60;
    R = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', o);
    assert_isequal(size(y), [527617           2]);
    assert_isequal(y(1), 0);
end
%=============================================================================

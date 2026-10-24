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
% The URL answers with a 302 redirect to a PNG served as image/png:
% webread must follow the redirect and decode the image from the Content-Type
% header, without any ContentType option.
url = 'https://github.com/nelson-lang/nelson/raw/master/resources/fibonacci.png';
try
  img = webread(url, weboptions('Timeout', 30));
catch ex
  transient = strcmp(ex.message, _('Forbidden (403)')) || ...
              strcmp(ex.message, _('Timeout was reached')) || ...
              strcmp(ex.message, _('Couldn''t resolve host name')) || ...
              strcmp(ex.message, _('Service unavailable (503)'));
  if transient
    skip_testsuite(transient, ex.message);
  end
  rethrow(ex);
end
ref = imread([nelsonroot(), '/resources/fibonacci.png']);
asserts.isequal(class(img), 'uint8');
asserts.isequal(img, ref);
%=============================================================================
% Same image fetched through a redirect, with the redirect disabled: the
% redirect page itself is returned, not the image.
r = webread(url, weboptions('Timeout', 30, 'FollowLocation', false));
asserts.isfalse(isequal(class(r), 'uint8') && isequal(size(r), size(ref)));
%=============================================================================

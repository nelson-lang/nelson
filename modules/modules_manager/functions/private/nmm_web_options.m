%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = nmm_web_options()
  % weboptions used to download a registry index or a package archive. Follow
  % HTTP redirects (a GitHub release URL 302-redirects to
  % objects.githubusercontent.com, so without this the download saves an empty
  % body and the checksum fails) and allow enough time for a several-megabyte
  % archive over a slow link.
  options = weboptions('FollowLocation', true, 'Timeout', 120);
end
%=============================================================================

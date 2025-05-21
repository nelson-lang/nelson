%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('webtools', 'builtin'), 'webtools');
addpath(modulepath('webtools', 'functions'), '-frozen');
%=============================================================================
if (isempty(getenv('GIT_SSL_CAINFO')))
  % Set the SSL CAINFO environment variable for Git
  % This is required to avoid SSL certificate issues when using Git
  % with HTTPS URLs.
  % The path to the cacert.pem file is set to the resources directory
  % of the webtools module.
  setenv('GIT_SSL_CAINFO', [modulepath('webtools', 'root'), '/resources/cacert.pem']);
end
%=============================================================================

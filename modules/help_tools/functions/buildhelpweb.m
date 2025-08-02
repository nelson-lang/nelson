%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function buildhelpweb(varargin)
  % buildhelpweb(destdir, [currentlang])
  narginchk(1, 2);
  
  if nargin() == 2
    lang = varargin{2};
  else
    lang = getdefaultlanguage();
  end
  
  dirdest = varargin{1};
  if ~isdir(dirdest)
    error(_('Existing directory expected.'));
  end
  
  srclist = {};
  run([nelsonroot() '/modules/' 'modules.m']);
  funcList = str2func('@(x) x{1}');
  modules_help_list = string(cellfun(funcList, modules_list, 'UniformOutput', false));
  for module = modules_help_list(:)'
    src = [nelsonroot(), '/modules/', module{1}, '/help/', lang, '/xml'];
    if isdir(src)
      srclist{end + 1} = src;
    end
  end
  vervec = version('-number');
  title = ['Nelson', ' ', int2str(vervec(1)), '.', int2str(vervec(2)), '.', int2str(vervec(3)), '.', int2str(vervec(4))];
  xmldoctohtml(srclist, dirdest, title);
  
end
%=============================================================================

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
 
  modules_help_list = get_modules_help_list();
  for m = modules_help_list'
    help_file = [nelsonroot() '/modules/' m{1} '/help/' nelsonappid() '.modules.' m{1} '.help.' lang '.nhz'];
    if ~isfile(help_file)
      help_file = [nelsonroot() '/modules/' m{1} '/help/' nelsonappid() '.modules.' m{1} '.help.' getdefaultlanguage() '.nhz'];
    end
    unzip(help_file, dirdest);
  end
  lang_used = lang;
  main_help_file = [nelsonroot() '/modules/main/help/' nelsonappid() '.modules.main.help.' lang_used '.nhz'];
  if ~isfile(main_help_file)
    lang_used = getdefaultlanguage();
    main_help_file = [nelsonroot() '/modules/main/help/' nelsonappid() '.modules.main.help.' lang_used '.nhz'];
  end
  unzip(main_help_file, dirdest);
  d = dir([dirdest, '/main/*.*']);
  for k = 1:length(d)
    if ~d(k).isdir
      copyfile([dirdest, '/main/' , d(k).name], dirdest);
    end
  end
  rmdir([dirdest, '/main'], 's');
end
%=============================================================================

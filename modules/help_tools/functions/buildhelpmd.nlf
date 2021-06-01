%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
function buildhelpmd(varargin)
% build Nelson help files as markdown
% buildhelpmd(destdir) build help of Nelson
% buildhelpmd(destdir, module_name) build help of a module loaded in Nelson
%
  if (nargin() > 2 || nargin() < 1)
    error(_('Wrong number of input arguments.'));
  end
  destinationdir = varargin{1};
  if nargin() == 1
    helpForNelsonOnly(destinationdir);
    run([nelsonroot() '/modules/' 'modules.nls']);
    locales = {};
    for m = modules_help_list(:)'
      module_path = [nelsonroot() '/modules/' m{1}];
      l = buildHelpFromPath(destinationdir, m{1}, module_path);
      locales = [locales, l];
    end
    modules = modules_help_list;
  else
    module = varargin{2};
    module_path = modulepath(module);
    locales = buildHelpFromPath(destinationdir, module, module_path);
    modules = {module};
  end
  generatesLANGSmd(destinationdir, locales);
  buildSummary(destinationdir, locales, modules);
end
%=============================================================================
function locales = buildHelpFromPath(destinationdir, module, module_path)
  locales = {};
  for k = getavailablelanguages()(:)'
    locale = k{1}(1:2);
    src = [module_path, '/help/', k{1}, '/xml'];
    if isdir(src)
      dstbuild = [destinationdir, '/', locale, '/', module, '/'];
      res = mkdir(dstbuild);
      if ~res
        error(msg);
      end
      p = xmldoctomd(src, dstbuild, module);
      if p
        disp(['help ''', module, ''' (', k{1}, ') generated.']);
      else
        disp([module, _(' file was not generated.')]);
      end
      locales{end + 1} = locale;
    end
  end
end
%=============================================================================
function generatesLANGSmd(destinationdir, locales)
  content = {'# Languages'; ''};
  done = {};
  for l = locales(:)'
    res = contains(done, l{1});
    if isempty(res) || ~all(res)
      content{end + 1} = ['* [', l{1} ,'](', l{1}, ')'];
      done{end + 1} = l{1};
    end
  end
  content{end + 1} = '';
  filewrite([destinationdir, '/LANGS.md'], content);
end
%=============================================================================
function buildSummary(destinationdir, locales, modules_help_list, is_nelson_help)
  for l = locales(:)'
    global_summary = {['# ', 'Summary']; ''};
    global_summary_path = [destinationdir, '/', l{1}, '/',  'SUMMARY.md'];
    for m = modules_help_list(:)'
      module_summary_path = [destinationdir, '/', l{1}, '/', m{1}, '/', 'SUMMARY.md'];
      content = fileread(module_summary_path, 'cell');
      content = strrep(content, '](', ['](./', m{1}, '/']);
      global_summary = [global_summary; {''}; content];
    end
    filewrite(global_summary_path, global_summary);
  end
end
%=============================================================================
function helpForNelsonOnly(destinationdir)
  src_files = {'homepage.md', 'banner_homepage.png'};
  dst_files = {'README.md', 'banner_homepage.png'};
  for k = getavailablelanguages()(:)'
    locale = k{1}(1:2);
    dir_dst = [destinationdir, '/', locale];
    dir_src = [nelsonroot(), '/modules/main/help/', k{1}, '/md'];
    mkdir(dir_dst);
    for k = 1:length(src_files)
      copyfile([dir_src, '/', src_files{k}], [dir_dst, '/', dst_files{k}]);
    end
    changelogs = {'CHANGELOG.md', 'CHANGELOG-0.1.x.md', 'CHANGELOG-0.2.x.md', 'CHANGELOG-0.3.x.md', 'CHANGELOG-0.4.x.md'};
    changelogs_dir = [dir_dst, '/changelogs'];
    mkdir(changelogs_dir);
    for k = 1:length(changelogs)
      copyfile([nelsonroot(), '/', changelogs{k}], [changelogs_dir, '/', changelogs{k}]);
    end

    licenses = {'GPL2.md', 'LGPL21.md', 'license.md'};
    licenses_dir = [dir_dst, '/license'];
    mkdir(licenses_dir);
    for k = 1:length(licenses)
      copyfile([dir_src, '/', licenses{k}], [licenses_dir, '/', licenses{k}]);
    end
    content = fileread([dir_dst, '/', 'README.md']);
    content = strrep(content, '(CHANGELOG.md)', '(./changelogs/CHANGELOG.md)');
    content = strrep(content, '(CHANGELOG-0.4.x.md)', '(./changelogs/CHANGELOG-0.4.x.md)');
    content = strrep(content, '(CHANGELOG-0.3.x.md)', '(./changelogs/CHANGELOG-0.3.x.md)');
    content = strrep(content, '(CHANGELOG-0.2.x.md)', '(./changelogs/CHANGELOG-0.2.x.md)');
    content = strrep(content, '(CHANGELOG-0.1.x.md)', '(./changelogs/CHANGELOG-0.1.x.md)');
    content = strrep(content, '(license.md)', '(./license/license.md)');
    filewrite([dir_dst, '/', 'README.md'], content);
  end
end
%=============================================================================

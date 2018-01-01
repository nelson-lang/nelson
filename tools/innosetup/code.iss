//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
procedure AfterNelsonInstall();
	var
		LanguageFileName : String;
		PreferencesDir : String;
		i : Integer;
		d : Integer;
		LanguageFileLines: TArrayOfString;
    InnosetupLanguage : String;
    Language : String;

	begin

    i := 0;
    setArrayLength(LanguageFileLines, 5);
    for d := 0 to GetArrayLength(LanguageFileLines)-1 do
      begin
        LanguageFileLines[d] := '';
      end;
		PreferencesDir := ExpandConstant('{userappdata}') + '\' + 'Nelson' + '\' + ExpandConstant('{#APPLICATION_VERSION}');
		LanguageFileName := preferencesDir + '\' + 'nelson.conf';
		if not DirExists(preferencesDir) then
			begin
				ForceDirectories(preferencesDir);
			end;
		if not FileExists(LanguageFileName) then	 
			begin
        InnosetupLanguage := ExpandConstant('{language}');

				Language := 'en_US';
        if CompareText(InnosetupLanguage, 'english') = 0 then
          begin
            Language := 'en_US';
          end;
        if CompareText(InnosetupLanguage, 'french') = 0 then
          begin
            Language := 'fr_FR';
          end;
        LanguageFileLines[i] := '{"language":"' + Language + '"}'; i := i + 1;
				SaveStringsToFile(LanguageFileName, LanguageFileLines, False);
			end;
	end;

#IFDEF UNICODE
  #DEFINE AW "W"
#ELSE
  #DEFINE AW "A"
#ENDIF
type
  INSTALLSTATE = Longint;
const
  INSTALLSTATE_INVALIDARG = -2;  // An invalid parameter was passed to the function.
  INSTALLSTATE_UNKNOWN = -1;     // The product is neither advertised or installed.
  INSTALLSTATE_ADVERTISED = 1;   // The product is advertised but not installed.
  INSTALLSTATE_ABSENT = 2;       // The product is installed for a different user.
  INSTALLSTATE_DEFAULT = 5;      // The product is installed for the current user.

  // Visual C++ 2015 Redistributable 14.0.23026
  VC_2015_REDIST_X86_MIN = '{A2563E55-3BEC-3828-8D67-E5E8B9E8B675}';
  VC_2015_REDIST_X64_MIN = '{0D3E9E15-DE7A-300B-96F1-B4AF12B96488}';

  VC_2015_REDIST_X86_ADD = '{BE960C1C-7BAD-3DE6-8B1A-2616FE532845}';
  VC_2015_REDIST_X64_ADD = '{BC958BD2-5DAC-3862-BB1A-C1BE0790438D}';

function MsiQueryProductState(szProduct: string): INSTALLSTATE; 
  external 'MsiQueryProductState{#AW}@msi.dll stdcall';

function VCVersionInstalled(const ProductID: string): Boolean;
begin
  Result := MsiQueryProductState(ProductID) = INSTALLSTATE_DEFAULT;
end;

function VCRedistNeedsInstall: Boolean;
begin
#ifdef NELSON_X64
  Result := not (VCVersionInstalled(VC_2015_REDIST_X64_MIN));
#else
  Result := not (VCVersionInstalled(VC_2015_REDIST_X86_MIN));
#endif
end;

Procedure URLLabelOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExec('open', 'https://nelson-numerical-software.github.io/nelson-website/', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

Procedure InitializeWizard;
var
  URLLabel: TNewStaticText;
begin
  URLLabel := TNewStaticText.Create(WizardForm);
  URLLabel.Caption := 'Nelson''s website';
  URLLabel.Cursor := crHand;
  URLLabel.OnClick := @URLLabelOnClick;
  URLLabel.Parent := WizardForm;
  { Alter Font *after* setting Parent so the correct defaults are inherited first }
  URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderline];
  URLLabel.Font.Color := clBlue;
  URLLabel.Top := WizardForm.ClientHeight - URLLabel.Height - 15;
  URLLabel.Left := ScaleX(20);
end;
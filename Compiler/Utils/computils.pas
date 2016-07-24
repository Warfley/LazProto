unit CompUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
   fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables};

type
  TVersionData = record
    CompanyName: String;
    FileDescription: String;
    FileVersion: String;
    InternalName: String;
    LegalCopyright: String;
    OriginalFilename: String;
    ProductName: String;
    ProductVersion: String;
  end;

function GetVersionData(Filename: String):TVersionData;
implementation

function GetVersionData(Filename: String):TVersionData;
var
  FileVerInfo: TFileVersionInfo;
begin

  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=Filename;
    FileVerInfo.ReadFileInfo;
    Result.CompanyName:=FileVerInfo.VersionStrings.Values['CompanyName'];
    Result.FileDescription:=FileVerInfo.VersionStrings.Values['FileDescription'];
    Result.FileVersion:=FileVerInfo.VersionStrings.Values['FileVersion'];
    Result.InternalName:=FileVerInfo.VersionStrings.Values['InternalName'];
    Result.LegalCopyright:=FileVerInfo.VersionStrings.Values['LegalCopyright'];
    Result.OriginalFilename:=FileVerInfo.VersionStrings.Values['OriginalFilename'];
    Result.ProductName:=FileVerInfo.VersionStrings.Values['ProductName'];
    Result.ProductVersion:=FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

end.


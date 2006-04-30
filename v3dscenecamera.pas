{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of "view3dscene".

  "view3dscene" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "view3dscene" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "view3dscene"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit V3DSceneCamera;

interface

uses VectorMath, VRMLNodes;

type
  TCameraSetting = (
    csCameraKind, csHomeCameraPos, csHomeCameraDir, csHomeCameraUp);
  TCameraSettings = set of TCameraSetting;

  TCameraSettingsOverride = record
    OverrideSettings: TCameraSettings;
    { From fields below, only those are meaningfull that have
      csXxx in OverrideSettings. Rest of them can have undefined value. }
    CameraKind: TVRMLCameraKind;
    HomeCameraPos: TVector3Single;
    HomeCameraDir: TVector3Single;
    HomeCameraUp: TVector3Single;
  end;

const
  { This is a value of TCameraSettingsOverride type that says to NOT
    override any camera setting. I.e. it has OverrideSettings = []. }
  CameraNoOverride: TCameraSettingsOverride =
  (
    OverrideSettings: [];
    CameraKind: ckPerspective;
    HomeCameraPos: (0, 0, 0);
    HomeCameraDir: (0, 0, 0);
    HomeCameraUp: (0, 0, 0);
  );

{ This modifies some of the parameters passed by reference using
  values specified as CameraOverride. Those variables that
  are specified in CameraOverride.OverrideSettings are modified. }
procedure ApplyOverride(const CameraOverride: TCameraSettingsOverride;
  var CameraKind: TVRMLCameraKind;
  var HomeCameraPos, HomeCameraDir, HomeCameraUp: TVector3Single);

implementation

procedure ApplyOverride(const CameraOverride: TCameraSettingsOverride;
  var CameraKind: TVRMLCameraKind;
  var HomeCameraPos, HomeCameraDir, HomeCameraUp: TVector3Single);
begin
 if csCameraKind    in CameraOverride.OverrideSettings then CameraKind    := CameraOverride.CameraKind;
 if csHomeCameraPos in CameraOverride.OverrideSettings then HomeCameraPos := CameraOverride.HomeCameraPos;
 if csHomeCameraDir in CameraOverride.OverrideSettings then HomeCameraDir := CameraOverride.HomeCameraDir;
 if csHomeCameraUp  in CameraOverride.OverrideSettings then HomeCameraUp  := CameraOverride.HomeCameraUp;
end;

end.
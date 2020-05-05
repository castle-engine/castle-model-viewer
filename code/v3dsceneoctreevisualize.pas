{
  Copyright 2002-2018 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Visualizing the octree. }
unit V3DSceneOctreeVisualize;

{$I v3dsceneconf.inc}

interface

uses CastleInternalOctree, CastleWindow, CastleScene;

type
  TOctreeDisplay = object
  private
    procedure AddDisplayStatus(var S: string);
  public
    { Constructor, initially display is none (Whole = @false and Depth = -1). }
    constructor Init(const AName: string);
  public
    Whole: boolean;

    { Depth to dispay. Meaningful only if Whole = false.
      -1 means "don't display octree". }
    Depth: Integer;

    Name: string;

    MenuWhole: TMenuItemChecked;

    procedure DoMenuIncDepth;
    procedure DoMenuDecDepth;
    procedure DoMenuToggleWhole;
  end;

var
  OctreeTrianglesDisplay: TOctreeDisplay;
  OctreeVisibleShapesDisplay: TOctreeDisplay;
  OctreeCollidableShapesDisplay: TOctreeDisplay;

procedure OctreeDisplay(Scene: TCastleScene);

function OctreeDisplayStatus: string;

implementation

uses SysUtils,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleColors, CastleGLUtils, CastleShapes, CastleGLBoxes, V3DSceneBoxes;

{ TOctreeDisplay ------------------------------------------------------------- }

constructor TOctreeDisplay.Init(const AName: string);
begin
  Whole := false;
  Depth := -1;
  Name := AName;
end;

procedure TOctreeDisplay.DoMenuIncDepth;
begin
  if Whole then
  begin
    Whole := false;
    MenuWhole.Checked := Whole;
    Depth := -1;
  end;

  Inc(Depth);
end;

procedure TOctreeDisplay.DoMenuDecDepth;
begin
  if Whole then
  begin
    Whole := false;
    MenuWhole.Checked := Whole;
    Depth := -1;
  end;

  Dec(Depth);
  if Depth < -1 then Depth := -1;
end;

procedure TOctreeDisplay.DoMenuToggleWhole;
begin
  Whole := not Whole;
  if not Whole then
    Depth := -1;
end;

procedure TOctreeDisplay.AddDisplayStatus(var S: string);
begin
  if Whole then
    S += Format(', Octree %s display: whole', [Name]) else
  if Depth <> -1 then
    S += Format(', Octree %s display: depth %d', [Name, Depth]);
end;

{ ---------------------------------------------------------------------------- }

procedure OctreeDisplay(Scene: TCastleScene);

(* TODO:
  This should be rewritten to construct a TCastleScene with TLineSet inside.
  Rendering using glDrawBox3DWire relies on OpenGL immediate mode,
  not possible under OpenGLES,
  cumbersome (needs PushMatrix hack in view3dscene.lpr)
  under modern OpenGL with EnabledFixedFunction = false.
*)

{$ifndef OpenGLES} //TODO-es

  procedure DisplayOctreeDepth(octreenode: TOctreeNode;
    OctreeDisplayDepth: integer);
  var
    b0, b1, b2: boolean;
  begin
    with octreenode do
      if Depth = OctreeDisplayDepth then
      begin
        if not (IsLeaf and (ItemsCount = 0)) then
          glDrawBox3DWire(Box);
      end else
      if not IsLeaf then
      begin
        for b0 := false to true do
          for b1 := false to true do
            for b2 := false to true do
              DisplayOctreeDepth(TreeSubNodes[b0, b1, b2], OctreeDisplayDepth);
      end;
  end;

  procedure DisplayOctreeTrianglesDepth(OctreeDisplayDepth: integer);
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    { Octree is not always ready, as it's recalculation during animations
      may hurt. Also, Draw may be called in various situations even when Scene
      is not really ready (e.g. when showing errors after scene loading).
      Also, octrees for particular shapes are not necessarily
      created, since some shapes may be not collidable.
      So we have to carefully check here whether appropriate things
      are initialized. }

    if Scene <> nil then
    begin
      ShapeList := Scene.Shapes.TraverseList(true);
      for Shape in ShapeList do
        if Shape.InternalOctreeTriangles <> nil then
        begin
          glPushMatrix;
            glMultMatrix(Shape.State.Transformation.Transform);
            DisplayOctreeDepth(Shape.InternalOctreeTriangles.TreeRoot,
              OctreeDisplayDepth);
          glPopMatrix;
        end;
    end;
  end;

  procedure DisplayOctreeWhole(OctreeNode: TOctreeNode);
  var
    b0, b1, b2: boolean;
  begin
    with OctreeNode do
    begin
      if not (IsLeaf and (ItemsCount = 0)) then
        glDrawBox3DWire(Box);

      if not IsLeaf then
      begin
        for b0 := false to true do
          for b1 := false to true do
            for b2 := false to true do
              DisplayOctreeWhole(TreeSubNodes[b0, b1, b2]);
      end;
    end;
  end;

  procedure DisplayOctreeTrianglesWhole;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    { Octree is not always ready, as it's recalculation during animations
      may hurt. Also, Draw may be called in various situations even when Scene
      is not really ready (e.g. when showing errors after scene loading).
      Also, octrees for particular shapes are not necessarily
      created, since some shapes may be not collidable.
      So we have to carefully check here whether appropriate things
      are initialized. }

    if Scene <> nil then
    begin
      ShapeList := Scene.Shapes.TraverseList(true);
      for Shape in ShapeList do
        if Shape.InternalOctreeTriangles <> nil then
        begin
          glPushMatrix;
            glMultMatrix(Shape.State.Transformation.Transform);
            DisplayOctreeWhole(Shape.InternalOctreeTriangles.TreeRoot);
          glPopMatrix;
        end;
    end;
  end;

begin
  if OctreeTrianglesDisplay.Whole then
  begin
    glColorv(Yellow);
    DisplayOctreeTrianglesWhole;
  end else
  if OctreeTrianglesDisplay.Depth >= 0 then
  begin
    glColorv(Yellow);
    DisplayOctreeTrianglesDepth(OctreeTrianglesDisplay.Depth);
  end;

  if OctreeVisibleShapesDisplay.Whole then
  begin
    glColorv(Blue);
    DisplayOctreeWhole(Scene.InternalOctreeRendering.TreeRoot);
  end else
  if OctreeVisibleShapesDisplay.Depth >= 0 then
  begin
    glColorv(Blue);
    DisplayOctreeDepth(Scene.InternalOctreeRendering.TreeRoot,
      OctreeVisibleShapesDisplay.Depth);
  end;

  if OctreeCollidableShapesDisplay.Whole then
  begin
    glColorv(Red);
    DisplayOctreeWhole(Scene.InternalOctreeDynamicCollisions.TreeRoot);
  end else
  if OctreeCollidableShapesDisplay.Depth >= 0 then
  begin
    glColorv(Red);
    DisplayOctreeDepth(Scene.InternalOctreeDynamicCollisions.TreeRoot,
      OctreeCollidableShapesDisplay.Depth);
  end;
{$else}
begin
{$endif}
end;

function OctreeDisplayStatus: string;
begin
  Result := '';
  OctreeTrianglesDisplay       .AddDisplayStatus(Result);
  OctreeVisibleShapesDisplay   .AddDisplayStatus(Result);
  OctreeCollidableShapesDisplay.AddDisplayStatus(Result);
end;

initialization
  OctreeTrianglesDisplay       .Init('triangles');
  OctreeVisibleShapesDisplay   .Init('visible shapes');
  OctreeCollidableShapesDisplay.Init('collidable shapes');
end.

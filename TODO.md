- rename `ObjectId` to `SceneId`
- Main.idr, Game.idr
- rendering code into Draw.idr

# Resources and community

All resources go into "res/", the main game resources are in "res/main/", community resources are in "res/community/name/"

Make a ResourceId type that uniquely identifies a resource, and `filepath : ResourceId -> String`

module AIEvents

import Common

-- These are input events for the AI, which produces scene events

public export
data Event = CollisionStart CollisionForObject CollisionForObject
           | CollisionStop CollisionForObject CollisionForObject

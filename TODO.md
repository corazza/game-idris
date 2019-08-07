# Creations / MapDescription

Separate server creation and map creation. Map creations are either static or dynamic

The real separation between creations are static and dynamic: those the client can load by itself, and those that must be sent over by the server.

Wall should remain separate in the map description, but should also be just normal creations. Walls should also be normal objects that are merely parametrized by their creations. Which means ObjectDescriptions must be parametrizable.

# To abstract

Id store (decideId, newId (priv)), parametrizes on id type

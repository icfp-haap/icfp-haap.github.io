--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.PlayerAttr
Description : Atributos dos personagens.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo define um tipo que representa as capacidades (atributos) de um
personagem, e.g. a força do personagem. Também disponibilizia uma lista com as
capacidades das personagens disponíveis no jogo.
-}
module Sokoban.Game.PlayerAttr (
    PlayerAttr (..),
    attributes
    ) where

--------------------------------------------------------------------------------

-- | Representa as capacidades e outras informações sobre um personagem.
data PlayerAttr = PlayerAttr {
    name         :: String, -- ^ Nome do personagem.
    description  :: String, -- ^ Descrição do personagem.
    pushStrength :: Int,    -- ^ Número de caixas que o personagem pode empurrar
                            -- simultaneamente.
    canPull      :: Bool,   -- ^ Determina se o personagem pode puxar caixas.
    canTeleport  :: Bool,   -- ^ Determina se o personagem se pode teleportar.
    bitmapName   :: String  -- ^ Identificador dos bitmaps do personagem.
    }

-- | Atributos dos personagens disponíveis no jogo.
attributes :: [PlayerAttr]
attributes = [
    PlayerAttr {
        name         = "Sokoban",
        description  = "Um tipo normal. Consegue\n\
                       \empurrar uma caixa de cada\n\
                       \vez.",
        pushStrength = 1,
        canPull      = False,
        canTeleport  = False,
        bitmapName   = "sokoban"
        },
    PlayerAttr {
        name         = "The Hulk",
        description  = "Mais forte que a média,\n\
                       \consegue empurrar até 3 caixas\n\
                       \de uma só vez.",
        pushStrength = 3,
        canPull      = False,
        canTeleport  = False,
        bitmapName   = "the_hulk"
        },
    PlayerAttr {
        name         = "Captain Hook",
        description  = "Com um gancho no lugar da mão\n\
                       \direita, pode puxar caixas.\n\
                       \[Espaço] + [setas] para agarrar\n\
                       \uma caixa.",
        pushStrength = 1,
        canPull      = True,
        canTeleport  = False,
        bitmapName   = "captain_hook"
        },
    PlayerAttr {
        name         = "Teleporting Girl",
        description  = "Consegue teleportar-se para\n\
                       \sítios por onde já passou.\n\
                       \[S] para gravar a localização,\n\
                       \[X] para teleportar.",
        pushStrength = 1,
        canPull      = False,
        canTeleport  = True,
        bitmapName   = "teleporting_girl"
        }
    ]

--------------------------------------------------------------------------------

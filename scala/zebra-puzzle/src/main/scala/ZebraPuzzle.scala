import scala.collection.mutable.ListBuffer
object ZebraPuzzle:

  case class EntityIntel(
    var resident: Option[Resident] = None,
    var houseColor: Option[HouseColor] = None,
    var beverage: Option[Beverage] = None,
    var hobby: Option[Hobby] = None,
    var pet: Option[Pet] = None
  )

  enum Resident:
    case Englishman, Spaniard, Ukrainian, Norwegian, Japanese

  enum HouseColor:
    case Red, Green, Ivory, Yellow, Blue

  enum Beverage:
    case Coffee, Tea, Milk, OrangeJuice, Water

  enum Hobby:
    case Dance, Paint, Read, Football, Chess

  enum Pet:
    case Dog, Snail, Fox, Horse, Zebra

  case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  lazy val solve: Solution = {
    case class Context(entity: EntityIntel, index: Int, houses: Map[Int, EntityIntel])
    val clues = List[Context => (Option[EntityIntel], Map[Int, EntityIntel])](
      c => { c match
        // 2. The Englishman lives in the red house.
        case Context(entity: EntityIntel(resident: Some(Englishman))) => (Some(c.entity.copy(houseColor = Some(Red))), Map.empty)
        case Context(entity: EntityIntel(houseColor: Some(Red))) => (Some(c.entity.copy(resident = Some(Englishman))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 3. The Spaniard owns the dog.
        case Context(entity: EntityIntel(resident: Some(Spaniard))) => (Some(c.entity.copy(pet = Some(Dog))), Map.empty)
        case Context(entity: EntityIntel(pet: Some(Dog))) => (Some(c.entity.copy(resident = Some(Spaniard))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 4. The person in the green house drinks coffee.
        case Context(entity: EntityIntel(houseColor: Some(Green))) => (Some(c.entity.copy(beverage = Some(Coffee))), Map.empty)
        case Context(entity: EntityIntel(beverage: Some(Coffee))) => (Some(c.entity.copy(houseColor = Some(Green))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 5. The Ukrainian drinks tea.
        case Context(entity: EntityIntel(resident: Some(Ukrainian))) => (Some(c.entity.copy(beverage = Some(Tea))), Map.empty)
        case Context(entity: EntityIntel(beverage: Some(Tea))) => (Some(c.entity.copy(resident = Some(Ukrainian))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 6. The green house is immediately to the right of the ivory house.
        case Context(entity: EntityIntel(houseColor: Some(Green))) => (None,
          c.houses
            .find(_._1 == c.index - 1)
            .map(_._1 -> _._2.copy(houseColor = Ivory))
            .toMap)
        case Context(entity: EntityIntel(houseColor: Some(Ivory))) => (None,
          c.houses
            .find(_._1 == c.index + 1)
            .map(_._1 -> _._2.copy(houseColor = Green))
            .toMap)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 7. The snail owner likes to go dancing.
        case Context(entity: EntityIntel(pet: Some(Snail))) => (Some(c.entity.copy(hobby = Some(Dance))), Map.empty)
        case Context(entity: EntityIntel(hobby: Some(Dance))) => (Some(c.entity.copy(pet = Some(Snail))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 8. The person in the yellow house is a painter.
        case Context(entity: EntityIntel(houseColor: Some(Yellow))) => (Some(c.entity.copy(hobby = Some(Painter))), Map.empty)
        case Context(entity: EntityIntel(hobby: Some(Paint))) => (Some(c.entity.copy(hobby = Some(Paint))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 9. The person in the middle house drinks milk.
        case Context(index: 2) => (Some(c.entity.copy(beverage = Some(Milk))), Map.empty)
        // no opposite
        case _ => (None, Map.empty)
      },
    
      c => { c match
        // 10. The Norwegian lives in the first house.
        case Context(index: 0) => (Some(c.entity.copy(resident = Some(Norwegian))), Map.empty)
        // no opposite
        case _ => (None, Map.empty)
      }

      c => { c match
        // 11. The person who enjoys reading lives in the house next to the person with the fox.
        case Context(entity: EntityIntel(hobby: Some(Read))) => (None, {
            val neighbors =
              c.houses
                .filter(_._1 == c.index - 1 || _._1 == c.index + 1)
            if (neighbors.exists(_._2.pet == Some(Fox)))
              Map.empty
            else
              val petLessNeighbors =
                neighbors
                  .filter(_._2.pet.isEmpty)
              if (petLessNeighbors.size != 1)
                Map.empty
              else
                petLessNeighbors
                  .map(_._1 -> _._2.copy(pet = Some(Fox)))
                  .toMap
        })
        case Context(entity: EntityIntel(pet: Some(Fox))) => (None, {
            val neighbors =
              c.houses
                .filter(_._1 == c.index - 1 || _._1 == c.index + 1)
            if (neighbors.exists(_._2.hobby == Some(Read)))
              Map.empty
            else
              val hobbyLessNeighbors =
                neighbors
                  .filter(_._2.hobby.isEmpty)
              if (hobbyLessNeighbors.size != 1)
                Map.empty
              else
                hobbyLessNeighbors
                  .map(_._1 -> _._2.copy(hobby = Some(Read)))
                  .toMap
        })
        case _ => (None, Map.empty)
      },
    
      c => { c match
        // 12. The painter's house is next to the house with the horse.
        case Context(entity: EntityIntel(hobby: Some(Paint))) => (None, {
            val neighbors =
              c.houses
                .filter(_._1 == c.index - 1 || _._1 == c.index + 1)
            if (neighbors.exists(_._2.pet == Some(Horse)))
              Map.empty
            else
              val petLessNeighbors =
                neighbors
                  .filter(_._2.pet.isEmpty)
              if (petLessNeighbors.size != 1)
                Map.empty
              else
                petLessNeighbors
                  .map(_._1 -> _._2.copy(pet = Some(Horse)))
                  .toMap
        })
        case Context(entity: EntityIntel(pet: Some(Horse))) => (None, {
            val neighbors =
              c.houses
                .filter(_._1 == c.index - 1 || _._1 == c.index + 1)
            if (neighbors.exists(_._2.hobby == Some(Paint)))
              Map.empty
            else
              val hobbyLessNeighbors =
                neighbors
                  .filter(_._2.hobby.isEmpty)
              if (hobbyLessNeighbors.size != 1)
                Map.empty
              else
                hobbyLessNeighbors
                  .map(_._1 -> _._2.copy(hobby = Some(Paint)))
                  .toMap
        })
        case _ => (None, Map.empty)
      },

      c => { c match
        // 13. The person who plays football drinks orange juice.
        case Context(entity: EntityIntel(hobby: Some(Football))) => (Some(c.entity.copy(beverage = Some(OrangeJuice))), Map.empty)
        case Context(entity: EntityIntel(beverage: Some(OrangeJuice))) => (Some(c.entity.copy(hobby = Some(Football))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 14. The Japanese person plays chess.
        case Context(entity: EntityIntel(resident: Some(Japanese))) => (Some(c.entity.copy(hohbby = Some(Chess))), Map.empty)
        case Context(entity: EntityIntel(hobby: Some(Chess))) => (Some(c.entity.copy(resident = Some(Japanese))), Map.empty)
        case _ => (None, Map.empty)
      },

      c => { c match
        // 15. The Norwegian lives next to the blue house.
        case Context(entity: EntityIntel(resident: Some(Norwegian))) => (None, {
            val neighbors =
              c.houses
                .filter(_._1 == c.index - 1 || _._1 == c.index + 1)
            if (neighbors.exists(_._2.houseColor == Some(Blue)))
              Map.empty
            else
              val houseColorLessNeighbors =
                neighbors
                  .filter(_._2.houseColor.isEmpty)
              if (houseColorLessNeighbors.size != 1)
                Map.empty
              else
                houseColorLessNeighbors
                  .map(_._1 -> _._2.copy(houseColor = Some(Blue)))
                  .toMap
        })
        case Context(entity: EntityIntel(houseColor: Some(Blue))) => (None, {
            val neighbors =
              c.houses
                .filter(_._1 == c.index - 1 || _._1 == c.index + 1)
            if (neighbors.exists(_._2.resident == Some(Norwegian)))
              Map.empty
            else
              val residentLessNeighbors =
                neighbors
                  .filter(_._2.resident.isEmpty)
              if (residentLessNeighbors.size != 1)
                Map.empty
              else
                residentLessNeighbors
                  .map(_._1 -> _._2.copy(resident = Some(Norwegian)))
                  .toMap
        })
        case _ => (None, Map.empty)
      }
    )
}
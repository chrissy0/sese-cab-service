package de.tuberlin.sese.cabservice.cab.location;

import de.tuberlin.sese.cabservice.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownSectionException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;
import java.util.Optional;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class CabLocationServiceTest {

    @Autowired
    private CabLocationService service;

    @MockBean
    private CabLocationRepo locationRepo;

    @MockBean
    private CabRepo cabRepo;

    @Captor
    private ArgumentCaptor<CabLocationEntity> entityCaptor;

    @Test
    public void shouldGetAllCabLocations() {
        CabLocationEntity entity1 = CabLocationEntity.builder()
                .cabId(1L)
                .section(12)
                .build();
        CabLocationEntity entity2 = CabLocationEntity.builder()
                .cabId(5L)
                .section(2)
                .build();

        when(locationRepo.findAll()).thenReturn(asList(entity1, entity2));

        List<CabLocationEntity> locations = service.getAllCabLocations();

        verify(locationRepo).findAll();
        verifyNoMoreInteractions(locationRepo);

        assertThat(locations)
                .usingRecursiveFieldByFieldElementComparator()
                .containsExactly(entity1, entity2);
    }

    @Test
    public void shouldSaveCabLocation() {
        when(cabRepo.findById(4L)).thenReturn(Optional.of(CabEntity.builder()
                .id(4L)
                .name("Some Cab Name")
                .build()));

        CabLocationEntity entity = CabLocationEntity.builder()
                .cabId(4L)
                .section(3)
                .build();

        service.saveCabLocation(entity);

        verify(locationRepo).findById(4L);
        verify(locationRepo).save(entityCaptor.capture());
        verifyNoMoreInteractions(locationRepo);

        assertThat(entityCaptor.getValue().getCabId()).isEqualTo(4);
        assertThat(entityCaptor.getValue().getSection()).isEqualTo(3);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionOnUnknownCabIdOnSaveCabLocation() {
        CabLocationEntity entity = CabLocationEntity.builder()
                .cabId(4L)
                .section(3)
                .build();

        assertThatThrownBy(() -> service.saveCabLocation(entity))
                .isInstanceOf(UnknownCabIdException.class);

        verifyNoMoreInteractions(locationRepo);
    }

    @Test
    public void shouldThrowUnknownSectionExceptionOnInvalidSectionOnSaveCabLocation() {
        when(cabRepo.findById(4L)).thenReturn(Optional.of(CabEntity.builder()
                .id(4L)
                .name("Some Cab Name")
                .build()));

        CabLocationEntity entity = CabLocationEntity.builder()
                .cabId(4L)
                .section(33)
                .build();

        assertThatThrownBy(() -> service.saveCabLocation(entity))
                .isInstanceOf(UnknownSectionException.class);

        verifyNoMoreInteractions(locationRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingEntityOnSaveCabLocation() {
        assertThatThrownBy(() -> service.saveCabLocation(null))
                .isExactlyInstanceOf(IllegalArgumentException.class);

        verifyNoMoreInteractions(locationRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCabIdOnSaveCabLocation() {
        CabLocationEntity entity = CabLocationEntity.builder()
                .section(3)
                .build();

        assertThatThrownBy(() -> service.saveCabLocation(entity))
                .isExactlyInstanceOf(IllegalArgumentException.class);

        verifyNoMoreInteractions(locationRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingSectionOnSaveCabLocation() {
        CabLocationEntity entity = CabLocationEntity.builder()
                .cabId(3L)
                .build();

        assertThatThrownBy(() -> service.saveCabLocation(entity))
                .isExactlyInstanceOf(IllegalArgumentException.class);

        verifyNoMoreInteractions(locationRepo);
    }
}
